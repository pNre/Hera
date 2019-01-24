open Async
open Core

module Dispatcher : Bot_module.Module.t = struct
  let load_subscriptions () =
    Db.Main.subscriptions () >>> Result.iter ~f:(List.iter ~f:Subscription.begin_checking_subscription)
  ;;

  (* Bot module *)
  let register () =
    Db.Main.open_connection ();
    ( Db.Main.create_tables ()
    >>> function Ok _ -> () | Error _ -> failwith "Error creating module_feeds tables" );
    load_subscriptions ()
  ;;

  let help () = "*RSS feeds*\n`fa [url]`\n`fr [url]`"

  let on_command ~chat_id ~text =
    match text with
    | t when String.is_prefix t ~prefix:"fa " ->
      let feed_url = String.chop_prefix_exn t ~prefix:"fa " in
      Subscription.add_subscription chat_id feed_url;
      true
    | t when String.is_prefix t ~prefix:"fu " ->
      let _feed_url = String.chop_prefix_exn t ~prefix:"fu " in
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
