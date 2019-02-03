open Async
open Core

module Dispatcher : Bot.Module.t = struct
  (* Utils *)
  let reply chat_id text =
    don't_wait_for (Telegram.send_message ~chat_id ~text ~parse_mode:None () >>| ignore)
  ;;

  (* Init *)
  let create_tables () =
    Db.create_tables ()
    >>> function Ok _ -> () | Error _ -> failwith "Error creating module_feeds tables"
  ;;

  let load_subscriptions () =
    let begin_checking_subscription subscription =
      Subscription.begin_checking_subscription
        subscription
        (reply (Int64.of_string subscription.subscriber_id))
    in
    Db.subscriptions () >>> Result.iter ~f:(List.iter ~f:begin_checking_subscription)
  ;;

  (* Bot module *)
  let register () =
    Db.open_connection ();
    create_tables ();
    load_subscriptions ()
  ;;

  let help () =
    "*RSS feeds*\n`fa [url]` - add a feed\n`fr [url]` - remove a feed\n`fl` - list feeds"
  ;;

  let on_update update =
    match Telegram.parse_update update with
    | `Command ("fa", feed_url :: _, chat_id, _) ->
      Subscription.add_subscription
        ~subscriber_id:chat_id
        ~feed_url
        ~reply:(reply chat_id);
      true
    | `Command ("fr", feed_url :: _, chat_id, _) ->
      Subscription.remove_subscription
        ~subscriber_id:chat_id
        ~feed_url
        ~reply:(reply chat_id);
      true
    | `Command ("fl", _, chat_id, _) ->
      Subscription.list_subscriptions ~subscriber_id:chat_id ~reply:(reply chat_id);
      true
    | _ -> false
  ;;
end
