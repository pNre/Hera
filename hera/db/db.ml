open Async
open Caqti_async
open Core

(* Types *)
module Types = struct
  include Types
end

(* Queries *)
module Queries = struct
  (* Types *)
  let select_subscription_type = Caqti_type.(tup4 int string string string)
  let insert_subscription_type = Caqti_type.(tup3 string string string)
  let sent_item_type = Caqti_type.string
  let preference_type = Caqti_type.(tup3 string string string)

  let select_market_position_type =
    Caqti_type.(tup2 (tup2 int string) (tup4 string string string string))
  ;;

  let insert_market_position_type = Caqti_type.(tup3 string string (tup3 string string string))

  let subscription_of_result (id, subscriber_id, type_id, feed_url) =
    Types.Subscription.make ~id ~subscriber_id ~type_id ~feed_url ()
  ;;

  let preference_of_result (owner_id, key, value) =
    Types.Preference.make ~owner_id ~key ~value
  ;;

  let market_position_of_result ((id, owner_id), (symbol, price, size, currency)) =
    Types.Market_position.make ~id ~owner_id ~symbol ~price ~size ~currency ()
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
      {| CREATE TABLE IF NOT EXISTS "sent_item" ("item_url" TEXT NOT NULL PRIMARY KEY) |}
    in
    let preferences =
      {| CREATE TABLE IF NOT EXISTS "preference" (
           "owner_id" VARCHAR(64) NOT NULL,
           "key" VARCHAR(32) NOT NULL,
           "value" TEXT DEFAULT NULL,
           CONSTRAINT preference_un UNIQUE (owner_id, key)) |}
    in
    let market_position =
      {| CREATE TABLE IF NOT EXISTS "market_position" (
           "id" INTEGER NOT NULL PRIMARY KEY,
           "owner_id" VARCHAR(64) NOT NULL,
           "symbol" VARCHAR(32) NOT NULL,
           "price" VARCHAR(64) NOT NULL,
           "size" VARCHAR(64) NOT NULL,
           "currency" VARCHAR(8) NOT NULL) |}
    in
    let market_position_owner_id_index =
      {| CREATE INDEX IF NOT EXISTS market_positon_owner ON market_position (owner_id) |}
    in
    [ enable_foreign_keys
    ; subscription
    ; subscription_subscriber_id_index
    ; subscription_feed_index
    ; sent_item
    ; preferences
    ; market_position
    ; market_position_owner_id_index
    ]
    |> List.map ~f:(Caqti_request.exec Caqti_type.unit)
  ;;

  (* Subscriptions *)
  let subscriptions_for_subscriber =
    Caqti_request.collect
      Caqti_type.string
      select_subscription_type
      {| SELECT *
           FROM subscription
           WHERE subscriber_id = ?
           ORDER BY feed_url ASC |}
  ;;

  let subscriptions =
    Caqti_request.collect
      Caqti_type.unit
      select_subscription_type
      {| SELECT *
           FROM subscription
           ORDER BY feed_url ASC |}
  ;;

  let find_subscription =
    Caqti_request.find
      Caqti_type.(tup2 string string)
      select_subscription_type
      {| SELECT *
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

  (* Market positions *)
  let market_positions =
    Caqti_request.collect
      Caqti_type.unit
      select_market_position_type
      {| SELECT * FROM market_position |}
  ;;

  let market_positions_for_owner =
    Caqti_request.collect
      Caqti_type.string
      select_market_position_type
      {| SELECT * FROM market_position WHERE owner_id = ? |}
  ;;

  let insert_market_position =
    Caqti_request.exec
      insert_market_position_type
      {| INSERT INTO market_position
           (owner_id, symbol, price, size, currency)
           VALUES
           (?, ?, ?, ?, ?) |}
  ;;

  let delete_market_position =
    Caqti_request.exec
      Caqti_type.(tup2 string int)
      {| DELETE FROM market_position WHERE owner_id = ? AND id = ? |}
  ;;

  (* Sent items *)
  let insert_sent_item =
    Caqti_request.exec sent_item_type {| INSERT INTO sent_item (item_url) VALUES (?) |}
  ;;

  let find_sent_item =
    Caqti_request.find
      sent_item_type
      Caqti_type.int
      {| SELECT COUNT(1)
           FROM sent_item
           WHERE item_url = ? |}
  ;;

  (* Preferences *)
  let find_preference =
    Caqti_request.find
      Caqti_type.(tup2 string string)
      preference_type
      {| SELECT *
           FROM preference
           WHERE owner_id = ? AND key = ? |}
  ;;

  let preferences =
    Caqti_request.collect
      Caqti_type.string
      preference_type
      {| SELECT *
           FROM preference
           WHERE owner_id = ? |}
  ;;

  let insert_preference =
    Caqti_request.exec
      preference_type
      {| INSERT INTO preference
           (owner_id, key, value)
           VALUES
           (?, ?, ?) |}
  ;;

  let delete_preference =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      {| DELETE FROM preference WHERE owner_id = ? AND key = ? |}
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
    >>= function
    | Ok conn -> fn conn
    | Error err -> Deferred.return (Error err)
  in
  Throttle.enqueue sequencer f
;;

let in_transaction fn =
  let in_transaction' (module Connection : Caqti_async.CONNECTION) =
    Connection.start ()
    >>=? (fun _ -> fn (module Connection : Caqti_async.CONNECTION))
    >>= function
    | Ok _ -> Connection.commit ()
    | Error _ -> Connection.rollback ()
  in
  with_connection in_transaction'
;;

let string_of_error = Caqti_error.show

(* Database initialization *)
let create_tables () =
  let ignore_result_values results =
    results
    |> List.map ~f:(Result.map ~f:ignore)
    |> List.fold_result ~init:() ~f:(fun _ r -> r)
  in
  let exec_query q (module Connection : Caqti_async.CONNECTION) = Connection.exec q () in
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
    Connection.find_opt Queries.find_subscription (subscriber_id, feed_url)
  in
  with_connection subscription >>|? Option.map ~f:Queries.subscription_of_result
;;

let find_sent_item ~id =
  let sent_item' (module Connection : Caqti_async.CONNECTION) =
    Connection.find Queries.find_sent_item id
  in
  with_connection sent_item' >>|? fun x -> x > 0
;;

let find_preference ~owner_id ~key =
  let preference (module Connection : Caqti_async.CONNECTION) =
    Connection.find_opt Queries.find_preference (owner_id, key)
  in
  with_connection preference >>|? Option.map ~f:Queries.preference_of_result
;;

let preferences owner_id =
  let preferences' (module Connection : Caqti_async.CONNECTION) =
    Connection.collect_list Queries.preferences owner_id
  in
  with_connection preferences' >>|? List.map ~f:Queries.preference_of_result
;;

let market_positions () =
  let market_positions' (module Connection : Caqti_async.CONNECTION) =
    Connection.collect_list Queries.market_positions ()
  in
  with_connection market_positions' >>|? List.map ~f:Queries.market_position_of_result
;;

let market_positions_for_owner owner_id =
  let market_positions' (module Connection : Caqti_async.CONNECTION) =
    Connection.collect_list Queries.market_positions_for_owner owner_id
  in
  with_connection market_positions' >>|? List.map ~f:Queries.market_position_of_result
;;

(* Insert *)
let insert_sent_item ~id =
  let insert (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.insert_sent_item id
  in
  in_transaction insert
;;

let insert_subscription ~subscriber_id ~type_id ~feed_url =
  let insert (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.insert_subscription (subscriber_id, type_id, feed_url)
  in
  in_transaction insert
;;

let insert_preference ~owner_id ~key ~value =
  let insert (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.delete_preference (owner_id, key)
    >>=? fun _ -> Connection.exec Queries.insert_preference (owner_id, key, value)
  in
  in_transaction insert
;;

let insert_market_position ~owner_id ~symbol ~price ~size ~currency =
  let insert (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.insert_market_position (owner_id, symbol, (price, size, currency))
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

let delete_preference ~owner_id ~key =
  let delete (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.delete_preference (owner_id, key)
  in
  in_transaction delete
;;

let delete_market_position ~owner_id ~id =
  let delete (module Connection : Caqti_async.CONNECTION) =
    Connection.exec Queries.delete_market_position (owner_id, id)
  in
  in_transaction delete
;;
