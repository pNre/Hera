open Async
open Core

module Dispatcher : Bot_module.Module.t = struct
  (* Utils *)
  let reply chat_id text =
    don't_wait_for (Telegram.send_message ~chat_id ~text ~parse_mode:None () >>| ignore)
  ;;

  (* Init *)
  let create_tables () =
    Db.Main.create_tables ()
    >>> function Ok _ -> () | Error _ -> failwith "Error creating module_feeds tables"
  ;;

  let load_subscriptions () =
    let begin_checking_subscription subscription =
      Subscription.begin_checking_subscription
        subscription
        (reply (Int64.of_string subscription.subscriber_id))
    in
    Db.Main.subscriptions ()
    >>> Result.iter ~f:(List.iter ~f:begin_checking_subscription)
  ;;

  (* Bot module *)
  let register () =
    Db.Main.open_connection ();
    create_tables ();
    load_subscriptions ()
  ;;

  let help () = "*RSS feeds*\n`fa [url]`\n`fr [url]`\n`fl`"

  let on_command ~chat_id ~text =
    match text with
    | t when String.is_prefix t ~prefix:"fa " ->
      let feed_url = String.chop_prefix_exn t ~prefix:"fa " in
      Subscription.add_subscription
        ~subscriber_id:chat_id
        ~feed_url
        ~reply:(reply chat_id);
      true
    | t when String.is_prefix t ~prefix:"fr " ->
      let feed_url = String.chop_prefix_exn t ~prefix:"fr " in
      Subscription.remove_subscription
        ~subscriber_id:chat_id
        ~feed_url
        ~reply:(reply chat_id);
      true
    | t when String.is_prefix t ~prefix:"fl" ->
      Subscription.list_subscriptions ~reply:(reply chat_id);
      true
    | _ -> false
  ;;

  let on_update _reqd update =
    match update with
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some text; _}; _} ->
      on_command ~chat_id ~text
    | _ -> false
  ;;
end
