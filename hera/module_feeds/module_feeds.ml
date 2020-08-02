open Async
open Core

(* Utils *)
let message_preferences chat_id =
  let open Db.Types.Preference in
  let open User_preference in
  let key = key Show_links_preview in
  Db.find_preference ~owner_id:(Int64.to_string chat_id) ~key
  >>|? Option.value_map
         ~f:(fun p -> value_of_string Show_links_preview p.value)
         ~default:(default_value Show_links_preview)
  >>| function
  | Ok (Bool x) -> not x
  | _ -> false
;;

let reply chat_id text =
  message_preferences chat_id
  >>= (fun disable_web_page_preview ->
        Telegram.send_message ~chat_id ~text ~parse_mode:None ~disable_web_page_preview ()
        >>| ignore)
  |> don't_wait_for
;;

(* Init *)
let create_tables () =
  Db.create_tables ()
  >>> function
  | Ok _ -> ()
  | Error e ->
    failwith (sprintf "Error creating module_feeds tables: %s" (Db.string_of_error e))
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
    Subscription.add_subscription ~subscriber_id:chat_id ~feed_url ~reply:(reply chat_id);
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
