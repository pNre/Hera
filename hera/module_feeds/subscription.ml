open Async
open Core
open Types

let subscription_tasks = String.Table.create ~growth_allowed:true ()

let send_content_if_needed content ~subscription ~send =
  let wrap_error e = `Db e in
  match content with
  | Some {title; link; _} ->
    Logging.Module.info
      "Checking whether to send %s to %s"
      link
      subscription.subscriber_id;
    let sent_item = Types.{subscription_id = subscription.id; last_item_url = link} in
    Db.find_sent_item sent_item
    >>| Result.map_error ~f:wrap_error
    >>|? not
    >>|? Result.ok_if_true ~error:`Sent
    >>| Result.join
    >>=? (fun _ ->
           Logging.Module.info "Sending %s to %s" link subscription.subscriber_id;
           Db.insert_sent_item sent_item >>| Result.map_error ~f:wrap_error )
    >>> Result.iter ~f:(fun _ -> send (sprintf "%s\n%s" title link))
  | _ -> ()
;;

let begin_checking_subscription subscription send =
  let check_for_new_entries () =
    Http.request `GET (Uri.of_string subscription.feed_url) ()
    >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
    >>| Result.map_error ~f:(fun err -> `Http err)
    >>| Result.bind ~f:(Feed.parse ~xmlbase:(Uri.of_string subscription.feed_url))
    >>| Result.map ~f:(send_content_if_needed ~subscription ~send)
    >>| function
    | Ok _ -> ()
    | Error (`Http (Request exn)) ->
      Logging.Module.error
        "Download of feed %s failed -> %s"
        subscription.feed_url
        (Exn.to_string exn)
    | Error (`Http _err) ->
      Logging.Module.error "Download of feed %s failed" subscription.feed_url
    | Error (`Parse (_, error_string)) ->
      Logging.Module.error
        "Parse of feed %s failed -> %s"
        subscription.feed_url
        error_string
  in
  Logging.Module.info "Starting subscription checking task for %s" subscription.feed_url;
  let key = key_of_subscription subscription in
  let cancellation = Ivar.create () in
  Hashtbl.set subscription_tasks ~key ~data:cancellation;
  let timespan = Time.Span.create ~min:5 () in
  Clock.every'
    ~stop:(Ivar.read cancellation)
    ~continue_on_error:true
    timespan
    check_for_new_entries
;;

let stop_checking_subscription subscription =
  subscription
  |> key_of_subscription
  |> Hashtbl.find_and_remove subscription_tasks
  |> Option.iter ~f:(fun i -> Ivar.fill i ())
;;

let add_subscription ~subscriber_id ~feed_url ~reply =
  let feed_uri = Uri.of_string feed_url in
  match Uri.host feed_uri with
  | Some _ ->
    let subscription =
      Types.
        { id = 0
        ; subscriber_id = Int64.to_string subscriber_id
        ; type_id = "Telegram"
        ; feed_url }
    in
    let map_db_error e = `Db e in
    Db.find_subscription ~subscriber_id:subscription.subscriber_id ~feed_url
    >>| Result.map_error ~f:map_db_error
    >>|? not
    >>|? Result.ok_if_true ~error:`Result
    >>| Result.join
    >>=? (fun _ ->
           Logging.Module.info "Adding subscription %s" feed_url;
           Db.insert_subscription subscription >>| Result.map_error ~f:map_db_error )
    >>> Result.iter ~f:(fun () -> begin_checking_subscription subscription reply)
  | _ -> Logging.Module.error "Invalid uri %s" feed_url
;;

let remove_subscription ~subscriber_id ~feed_url ~reply =
  Logging.Module.info
    "Removing subscription %s for %s"
    feed_url
    (Int64.to_string subscriber_id);
  Db.delete_subscription ~subscriber_id:(Int64.to_string subscriber_id) ~feed_url
  >>> fun _ ->
  stop_checking_subscription
    {id = 0; subscriber_id = Int64.to_string subscriber_id; type_id = ""; feed_url};
  reply "Feed removed"
;;

let list_subscriptions ~subscriber_id ~reply =
  let send_subscriptions subscriptions =
    let text =
      if List.is_empty subscriptions
      then
        subscriptions
        |> List.map ~f:(fun subscription -> subscription.feed_url)
        |> String.concat ~sep:"\n"
      else "No subscriptions"
    in
    reply text
  in
  Db.subscriptions_for_subscriber (Int64.to_string subscriber_id)
  >>> Result.iter ~f:send_subscriptions
;;
