open Async
open Caqti_async
open Core

module Main = struct
  (* Queries *)
  module Queries = struct
    (* Types *)
    let select_subscription_type = Caqti_type.(tup4 int string string string)
    let insert_subscription_type = Caqti_type.(tup3 string string string)
    let sent_item_type = Caqti_type.(tup2 int string)

    let subscription_of_result (id, subscriber_id, type_id, feed_url) =
      Types.{id; subscriber_id; type_id; feed_url}
    ;;

    let sent_item_of_result (subscription_id, last_item_url) =
      Types.{subscription_id; last_item_url}
    ;;

    (* Initialization *)
    let create =
      let enable_foreign_keys = {| PRAGMA foreign_keys = ON |} in
      let subscription =
        {| CREATE TABLE IF NOT EXISTS "subscription" (
           "id" INTEGER NOT NULL PRIMARY KEY,
           "subscriber_id" VARCHAR(64) NOT NULL,
           "type_id" VARCHAR(32) NOT NULL,
           "feed_url" TEXT DEFAULT NULL,
           CONSTRAINT subscription_un UNIQUE (subscriber_id, type_id, feed_url)) |}
      in
      let subscription_subscriber_id_index =
        {| CREATE INDEX IF NOT EXISTS subscription_subscriber ON subscription (subscriber_id) |}
      in
      let subscription_feed_index =
        {| CREATE INDEX IF NOT EXISTS subscription_feed ON subscription (feed_url) |}
      in
      let sent_item =
        {| CREATE TABLE IF NOT EXISTS "sent_item" (
           "subscription_id" INTEGER NOT NULL REFERENCES subscription(id) ON DELETE CASCADE,
           "last_item_url" TEXT DEFAULT NULL,
           CONSTRAINT sent_item_un UNIQUE (subscription_id, last_item_url)) |}
      in
      let sent_item_subscription =
        {| CREATE INDEX IF NOT EXISTS sent_item_subscription ON sent_item (subscription_id) |}
      in
      [ enable_foreign_keys
      ; subscription
      ; subscription_subscriber_id_index
      ; subscription_feed_index
      ; sent_item
      ; sent_item_subscription ]
      |> List.map ~f:(Caqti_request.exec Caqti_type.unit)
    ;;

    (* Subscriptions *)
    let subscriptions_for_subscriber =
      Caqti_request.collect
        Caqti_type.string
        select_subscription_type
        {| SELECT *
           FROM subscription
           WHERE subscriber_id = ? |}
    ;;

    let subscriptions =
      Caqti_request.collect
        Caqti_type.unit
        select_subscription_type
        {| SELECT *
           FROM subscription |}
    ;;

    let find_subscription =
      Caqti_request.find
        Caqti_type.(tup2 string string)
        Caqti_type.int
        {| SELECT COUNT(1)
           FROM subscription
           WHERE subscriber_id = ? AND feed_url = ? |}
    ;;

    let insert_subscription =
      Caqti_request.exec
        insert_subscription_type
        {| INSERT INTO subscription
           (subscriber_id, type_id, feed_url)
           VALUES
           (?, ?, ?) |}
    ;;

    let delete_subscription =
      Caqti_request.exec
        Caqti_type.(tup2 string string)
        {| DELETE FROM subscription
           WHERE subscriber_id = ? AND feed_url = ? |}
    ;;

    (* Sent items *)
    let insert_sent_item =
      Caqti_request.exec
        sent_item_type
        {| INSERT INTO sent_item
           (subscription_id, last_item_url)
           VALUES
           (?, ?) |}
    ;;

    let find_sent_item =
      Caqti_request.find
        sent_item_type
        Caqti_type.int
        {| SELECT COUNT(1)
           FROM sent_item
           WHERE subscription_id = ? AND last_item_url = ? |}
    ;;
  end

  (* Connection *)
  let connection : (connection, Caqti_error.t) result Ivar.t = Ivar.create ()

  let open_connection () =
    let conn = "DB_CONNECTION_STRING" |> Sys.getenv_exn |> Uri.of_string |> connect in
    conn >>> Ivar.fill connection
  ;;

  let sequencer = Throttle.Sequencer.create ~continue_on_error:true ()

  let with_connection fn =
    let f () =
      Ivar.read connection
      >>= function Ok conn -> fn conn | Error err -> Deferred.return (Error err)
    in
    Throttle.enqueue sequencer f
  ;;

  let in_transaction fn =
    let in_transaction' (module Connection : Caqti_async.CONNECTION) =
      Connection.start ()
      >>=? (fun _ -> fn (module Connection : Caqti_async.CONNECTION))
      >>= function Ok _ -> Connection.commit () | Error _ -> Connection.rollback ()
    in
    with_connection in_transaction'
  ;;

  (* Database initialization *)
  let create_tables () =
    let ignore_result_values results =
      results
      |> List.map ~f:(Result.map ~f:ignore)
      |> List.fold_result ~init:() ~f:(fun _ r -> r)
    in
    let exec_query q (module Connection : Caqti_async.CONNECTION) =
      Connection.exec q ()
    in
    let queries =
      Queries.create
      |> List.map ~f:(fun q -> with_connection (exec_query q))
      |> Deferred.all
    in
    queries >>| ignore_result_values
  ;;

  (* Fetch *)
  let subscriptions_for_subscriber subscriber_id =
    let subscriptions' (module Connection : Caqti_async.CONNECTION) =
      Connection.collect_list Queries.subscriptions_for_subscriber subscriber_id
    in
    with_connection subscriptions' >>|? List.map ~f:Queries.subscription_of_result
  ;;

  let subscriptions () =
    let subscriptions' (module Connection : Caqti_async.CONNECTION) =
      Connection.collect_list Queries.subscriptions ()
    in
    with_connection subscriptions' >>|? List.map ~f:Queries.subscription_of_result
  ;;

  let find_subscription ~subscriber_id ~feed_url =
    let subscription (module Connection : Caqti_async.CONNECTION) =
      Connection.find Queries.find_subscription (subscriber_id, feed_url)
    in
    with_connection subscription >>|? fun x -> x > 0
  ;;

  let find_sent_item item =
    let sent_item' (module Connection : Caqti_async.CONNECTION) =
      Connection.find
        Queries.find_sent_item
        Types.(item.subscription_id, item.last_item_url)
    in
    with_connection sent_item' >>|? fun x -> x > 0
  ;;

  (* Insert *)
  let insert_sent_item item =
    let insert (module Connection : Caqti_async.CONNECTION) =
      Connection.exec
        Queries.insert_sent_item
        Types.(item.subscription_id, item.last_item_url)
    in
    in_transaction insert
  ;;

  let insert_subscription subscription =
    let insert (module Connection : Caqti_async.CONNECTION) =
      Connection.exec
        Queries.insert_subscription
        Types.(subscription.subscriber_id, subscription.type_id, subscription.feed_url)
    in
    in_transaction insert
  ;;

  (* Delete *)
  let delete_subscription ~subscriber_id ~feed_url =
    let delete (module Connection : Caqti_async.CONNECTION) =
      Connection.exec Queries.delete_subscription (subscriber_id, feed_url)
    in
    in_transaction delete
  ;;
end
