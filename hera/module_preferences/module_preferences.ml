open Async
open Core
open Db.Types

let map_preferences preferences =
  let set_key_values =
    List.filter_map
      preferences
      ~f:
        Preference.(
          fun pref ->
            User_preference.of_key pref.key
            |> Option.map ~f:(fun key ->
                 key, pref.value |> User_preference.value_of_string key))
  in
  let set_keys = set_key_values |> List.map ~f:(fun (k, _) -> k) in
  let missing_key_values =
    User_preference.all
    |> List.filter ~f:(Fn.non (List.mem set_keys ~equal:Poly.( = )))
    |> List.map ~f:(fun key -> key, User_preference.default_value key)
  in
  set_key_values @ missing_key_values
;;

let string_of_preference_key_value (key, value) =
  sprintf
    "`%s` = `%s`"
    (User_preference.description_of_preference key)
    (User_preference.string_of_value value)
;;

let list_preferences chat_id =
  don't_wait_for
    (Db.preferences (Int64.to_string chat_id)
    >>= function
    | Ok prefs ->
      let text =
        prefs
        |> map_preferences
        |> List.map ~f:string_of_preference_key_value
        |> String.concat ~sep:"\n"
      in
      Telegram.send_message ~chat_id ~text ()
    | Error e ->
      Logging.Module.error "%s" (Db.string_of_error e);
      Deferred.unit)
;;

let list_preference_options chat_id callback_path =
  let make_preference_button pref =
    let text = User_preference.description_of_preference pref in
    let key = User_preference.key pref in
    let sexp = sexp_of_list sexp_of_string [ callback_path; key ] in
    let data = sexp |> Sexp.to_string in
    Telegram.make_inline_keyboard_button ~text ~callback_data:(Some data) ()
  in
  let inline_keyboard =
    User_preference.all |> List.map ~f:(Fn.compose List.return make_preference_button)
  in
  let markup = Telegram.make_inline_keyboard_markup ~inline_keyboard () in
  let reply_markup =
    markup |> Telegram.jsonaf_of_inline_keyboard_markup |> Option.return
  in
  Telegram.send_message_don't_wait ~chat_id ~text:"Select one" ~reply_markup ()
;;

let set_preference chat_id key =
  let bool_inline_keyboard () =
    let data_for_value value =
      Sexp.List [ Atom "ps"; Atom key; sexp_of_bool value ]
      |> Sexp.to_string
      |> Option.return
    in
    let inline_keyboard =
      [ [ Telegram.make_inline_keyboard_button
            ~text:"Yes"
            ~callback_data:(data_for_value true)
            ()
        ]
      ; [ Telegram.make_inline_keyboard_button
            ~text:"No"
            ~callback_data:(data_for_value false)
            ()
        ]
      ]
    in
    Telegram.make_inline_keyboard_markup ~inline_keyboard ()
  in
  match User_preference.of_key key with
  | Some pref ->
    let inline_keyboard =
      match User_preference.type_of_preference pref with
      | `Bool -> bool_inline_keyboard ()
    in
    let reply_markup =
      inline_keyboard |> Telegram.jsonaf_of_inline_keyboard_markup |> Option.return
    in
    Telegram.send_message_don't_wait ~chat_id ~text:"Select one" ~reply_markup ()
  | None -> ()
;;

let set_preference_value chat_id key value =
  match User_preference.of_key key with
  | Some _ ->
    don't_wait_for
      (Db.insert_preference ~owner_id:(Int64.to_string chat_id) ~key ~value
      >>= function
      | Ok _ -> Telegram.send_message ~chat_id ~text:"Preference saved" () >>| ignore
      | Error _ ->
        Logging.Module.error "Couldn't save preference %s" key;
        Deferred.unit)
  | None -> ()
;;

let delete_preference chat_id key =
  don't_wait_for
    (Db.delete_preference ~owner_id:(Int64.to_string chat_id) ~key
    >>= function
    | Ok _ -> Telegram.send_message ~chat_id ~text:"Preference deleted" () >>| ignore
    | Error e ->
      Logging.Module.error
        "Couldn't delete preference %s -> %s"
        key
        (Db.string_of_error e);
      Deferred.unit)
;;

(* Bot module *)
let register () = ()

let help () =
  "*Preferences*\n\
   `pl` - list preferences\n\
   `ps` - set preference\n\
   `pd` - delete preference"
;;

let on_update update =
  match Telegram.parse_update update with
  | `Command ("pl", _, chat_id, _) ->
    list_preferences chat_id;
    true
  | `Command ("ps", _, chat_id, _) ->
    list_preference_options chat_id "ps";
    true
  | `Callback_query (Some (Sexp.List [ Atom "ps"; Atom key ]), chat_id, _) ->
    set_preference chat_id key;
    true
  | `Callback_query (Some (Sexp.List [ Atom "ps"; Atom key; Atom value ]), chat_id, _) ->
    set_preference_value chat_id key value;
    true
  | `Command ("pd", _, chat_id, _) ->
    list_preference_options chat_id "pd";
    true
  | `Callback_query (Some (Sexp.List [ Atom "pd"; Atom key ]), chat_id, _) ->
    delete_preference chat_id key;
    true
  | _ -> false
;;
