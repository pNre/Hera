open Async
open Core

module Dispatcher : Bot.Module.t = struct
  let list_preferences chat_id =
    don't_wait_for
      ( Db.preferences (Int64.to_string chat_id)
      >>= function
      | Ok prefs when List.length prefs > 0 ->
        let text =
          prefs
          |> List.map ~f:(fun (_, key, value) -> sprintf "`%s` = `%s`" key value)
          |> String.concat ~sep:"\n"
        in
        Telegram.send_message ~chat_id ~text ()
      | Ok _ -> Telegram.send_message ~chat_id ~text:"No preferences" ()
      | Error e ->
        Logging.Module.error "%s" (Caqti_error.show e);
        Deferred.unit )
  ;;

  let set_preference chat_id params =
    match params with
    | [key; value] ->
      don't_wait_for
        ( Db.insert_preference ~owner_id:(Int64.to_string chat_id) ~key ~value
        >>= function
        | Ok _ -> Telegram.send_message ~chat_id ~text:"Preference saved" () >>| ignore
        | Error _ ->
          Logging.Module.error "Couldn't save preference %s" key;
          Deferred.unit );
      true
    | _ -> false
  ;;

  let delete_preference chat_id key =
    don't_wait_for
      ( Db.delete_preference ~owner_id:(Int64.to_string chat_id) ~key
      >>= function
      | Ok _ -> Telegram.send_message ~chat_id ~text:"Preference deleted" () >>| ignore
      | Error e ->
        Logging.Module.error
          "Couldn't delete preference %s -> %s"
          key
          (Caqti_error.show e);
        Deferred.unit )
  ;;

  (* Bot module *)
  let register () = ()

  let help () =
    "*Preferences*\n\
     `pl` - list preferences\n\
     `ps [key] [value]` - set preference\n\
     `pd [key]` - delete preference"
  ;;

  let on_update update =
    match update with
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.Caseless.is_prefix t ~prefix:"pl" ->
      list_preferences chat_id;
      true
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.Caseless.is_prefix t ~prefix:"ps " ->
      set_preference chat_id (String.drop_prefix t 3 |> String.split ~on:' ')
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.Caseless.is_prefix t ~prefix:"pd " ->
      delete_preference chat_id (String.drop_prefix t 3 |> Caml.String.trim);
      true
    | _ -> false
  ;;
end
