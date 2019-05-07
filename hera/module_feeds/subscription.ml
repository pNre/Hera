open Async
open Core
open Types
open Db.Types

let subscription_tasks = String.Table.create ~growth_allowed:true ()

let send_content_if_needed content ~subscription ~send =
  let wrap_error e = `Db e in
  match content with
  | Some c ->
    let id = identifier_of_feed_content c in
    Logging.Module.info
      "Checking whether to send %s to %s"
      c.link
      subscription.Subscription.subscriber_id;
    Db.find_sent_item ~id
    >>| Result.map_error ~f:wrap_error
    >>|? not
    >>|? Result.ok_if_true ~error:`Sent
    >>| Result.join
    >>=? (fun _ ->
           Logging.Module.info
             "Sending %s to %s (%s)"
             c.link
             subscription.subscriber_id
             id;
           Db.insert_sent_item ~id >>| Result.map_error ~f:wrap_error)
    >>> Result.iter ~f:(fun _ -> send (sprintf "%s\n\n%s" c.title c.link))
  | _ -> ()
;;

let begin_checking_subscription subscription send =
  let check_for_new_entries () =
    Http.request `GET (Uri.of_string subscription.Subscription.feed_url) ()
    >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
    >>| Result.map_error ~f:(fun err -> `Http err)
    >>| Result.bind ~f:(Feed.parse ~xmlbase:(Uri.of_string subscription.feed_url))
    >>| Result.map ~f:(send_content_if_needed ~subscription ~send)
    >>| function
    | Ok _ -> ()
    | Error (`Http err) ->
      Logging.Module.error
        "Download of feed %s failed -> %s"
        subscription.feed_url
        (Http.string_of_error err)
    | Error (`Parse (_, error_string)) ->
      Logging.Module.error
        "Parse of feed %s failed -> %s"
        subscription.feed_url
        error_string
  in
  Logging.Module.info "Starting subscription checking task for %s" subscription.feed_url;
  let key = Subscription.key subscription in
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
  |> Subscription.key
  |> Hashtbl.find_and_remove subscription_tasks
  |> Option.iter ~f:(fun i -> Ivar.fill i ())
;;

let validate_subscription feed_url =
  Http.request `HEAD (Uri.of_string feed_url) ()
  >>| function
  | Ok (response, _) when Cohttp.Header.mem response.headers "content-length" ->
    Cohttp.Header.get response.headers "content-length"
    |> Option.map ~f:int_of_string_opt
    |> Option.join
    |> Option.value_map ~f:(fun x -> x < 1024 * 1024 * 10) ~default:false
    |> Result.ok_if_true ~error:`Too_big
  | _ -> Result.ok_unit
;;

let add_subscription ~subscriber_id ~feed_url ~reply =
  let feed_uri = Uri.of_string feed_url in
  match Uri.host feed_uri with
  | Some _ ->
    let subscriber_id = Int64.to_string subscriber_id in
    let map_db_error e = `Db e in
    Db.find_subscription ~subscriber_id ~feed_url
    >>| Result.map_error ~f:map_db_error
    >>| Result.map
          ~f:(Option.value_map ~f:(fun _ -> Error `Result) ~default:Result.ok_unit)
    >>| Result.join
    >>=? (fun _ -> validate_subscription feed_url)
    >>=? (fun _ ->
           Db.insert_subscription ~subscriber_id ~type_id:"" ~feed_url
           >>| Result.map_error ~f:map_db_error)
    >>=? (fun _ ->
           Db.find_subscription ~subscriber_id ~feed_url
           >>| Result.map_error ~f:map_db_error)
    >>> (function
    | Ok (Some subscription) -> begin_checking_subscription subscription reply
    | Error `Too_big ->
      don't_wait_for
        (Telegram.send_message
           ~chat_id:(Int64.of_string subscriber_id)
           ~text:"File too large"
           ())
    | _ -> ())
  | _ -> Logging.Module.error "Invalid uri %s" feed_url
;;

let remove_subscription ~subscriber_id ~feed_url ~reply =
  let subscriber_id = Int64.to_string subscriber_id in
  Logging.Module.info "Removing subscription %s for %s" feed_url subscriber_id;
  Db.delete_subscription ~subscriber_id ~feed_url
  >>> fun _ ->
  stop_checking_subscription (Subscription.make ~subscriber_id ~feed_url ());
  reply "Feed removed"
;;

let list_subscriptions ~subscriber_id ~reply =
  let send_subscriptions subscriptions =
    let text =
      if not (List.is_empty subscriptions)
      then
        subscriptions
        |> List.map ~f:(fun subscription -> subscription.Subscription.feed_url)
        |> String.concat ~sep:"\n"
      else "No subscriptions"
    in
    reply text
  in
  Db.subscriptions_for_subscriber (Int64.to_string subscriber_id)
  >>> Result.iter ~f:send_subscriptions
;;
