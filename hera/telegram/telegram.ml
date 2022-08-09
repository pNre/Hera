open Async
open Core
open Poly

type user =
  { id : int64
  ; is_bot : bool
  ; first_name : string
  ; last_name : string option [@jsonaf.option]
  ; username : string option [@jsonaf.option]
  ; language_code : string option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type chat =
  { id : int64
  ; chat_type : string [@key "type"]
  ; title : string option [@jsonaf.option]
  ; username : string option [@jsonaf.option]
  ; first_name : string option [@jsonaf.option]
  ; last_name : string option [@jsonaf.option]
  ; all_members_are_administrators : bool option [@jsonaf.option]
  ; description : string option [@jsonaf.option]
  ; invite_link : string option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type message_entity_type =
  | Mention [@name "mention"]
  | Hashtag [@name "hashtag"]
  | Cashtag [@name "cashtag"]
  | Bot_command [@name "bot_command"]
  | Url [@name "url"]
  | Email [@name "email"]
  | Phone_number [@name "phone_number"]
  | Bold [@name "bold"]
  | Italic [@name "italic"]
  | Code [@name "code"]
  | Pre [@name "pre"]
  | Text_link [@name "text_link"]
  | Text_mention [@name "text_mention"]
[@@deriving of_jsonaf]

type message_entity =
  { entity_type : string [@key "type"]
  ; offset : int
  ; length : int
  ; url : string option [@jsonaf.option]
  ; user : user option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type photo_size =
  { file_id : string
  ; width : int
  ; height : int
  ; file_size : int option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type message =
  { message_id : int64
  ; from : user
  ; date : int64
  ; chat : chat
  ; forward_from : user option [@jsonaf.option]
  ; forward_from_chat : chat option [@jsonaf.option]
  ; forward_from_message_id : int option [@jsonaf.option]
  ; forward_signature : string option [@jsonaf.option]
  ; forward_date : int option [@jsonaf.option]
  ; edit_date : int64 option [@jsonaf.option]
  ; media_group_id : string option [@jsonaf.option]
  ; author_signature : string option [@jsonaf.option]
  ; text : string option [@jsonaf.option]
  ; entities : message_entity list [@default []]
  ; caption_entities : message_entity list [@default []]
  ; caption : string option [@jsonaf.option]
  ; new_chat_members : user list [@default []]
  ; left_chat_member : user option [@jsonaf.option]
  ; new_chat_title : string option [@jsonaf.option]
  ; photo : photo_size list [@default []]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type inline_query =
  { id : int64
  ; from : user
  ; query : string
  ; offset : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type chosen_inline_result =
  { result_id : string
  ; from : user
  ; inline_message_id : string option [@jsonaf.option]
  ; query : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type callback_query =
  { id : string
  ; from : user
  ; message : message option [@jsonaf.option]
  ; inline_message_id : string option [@jsonaf.option]
  ; chat_instance : string
  ; data : string option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type update =
  { update_id : int64
  ; message : message option [@jsonaf.option]
  ; edited_message : message option [@jsonaf.option]
  ; channel_post : message option [@jsonaf.option]
  ; edited_channel_post : message option [@jsonaf.option]
  ; inline_query : inline_query option [@jsonaf.option]
  ; chosen_inline_result : chosen_inline_result option [@jsonaf.option]
  ; callback_query : callback_query option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type file =
  { file_id : string
  ; file_size : int option [@jsonaf.option]
  ; file_path : string option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type inline_keyboard_button =
  { text : string
  ; url : string option [@jsonaf.option]
  ; callback_data : string option [@default None] [@jsonaf.option]
  }
[@@deriving make, jsonaf_of] [@@jsonaf.allow_extra_fields]

type inline_keyboard_markup = { inline_keyboard : inline_keyboard_button list list }
[@@deriving make, jsonaf_of] [@@jsonaf.allow_extra_fields]

type send_message =
  { chat_id : string
  ; text : string
  ; parse_mode : string option [@default None] [@jsonaf.option]
  ; disable_web_page_preview : bool [@default false]
  ; reply_markup : Jsonaf.t option [@default None] [@jsonaf.option]
  }
[@@deriving make, jsonaf_of] [@@jsonaf.allow_extra_fields]

let token = Sys.getenv_exn "TELEGRAM_BOT_TOKEN"

type parse_mode =
  | Markdown
  | HTML

let string_of_parse_mode mode =
  match mode with
  | Markdown -> "Markdown"
  | HTML -> "HTML"
;;

let uri ?(base = "/") endpoint query =
  let endpoint =
    if String.is_prefix endpoint ~prefix:"/" then endpoint else "/" ^ endpoint
  in
  let path = base ^ "bot" ^ token ^ endpoint in
  Uri.make ~scheme:"https" ~host:"api.telegram.org" ~path ~query ()
;;

let set_webhook url =
  let qs = [ "url", [ Uri.to_string url ] ] in
  let uri = uri "setWebhook" qs in
  Http.request `GET uri ()
;;

let send_message
  ~chat_id
  ~text
  ?(parse_mode = Some Markdown)
  ?(disable_web_page_preview = false)
  ?(reply_markup = None)
  ()
  =
  let max_message_length = 4096 in
  let send_message' text =
    let parse_mode = parse_mode |> Option.map ~f:string_of_parse_mode in
    let body =
      make_send_message
        ~chat_id:(Int64.to_string chat_id)
        ~text
        ~disable_web_page_preview
        ~parse_mode
        ~reply_markup
        ()
      |> jsonaf_of_send_message
      |> Jsonaf.to_string
      |> Option.return
    in
    Logging.Main.info "%s" (Option.value_exn body);
    let uri = uri "sendMessage" [] in
    Http.request `POST uri ~body ~headers:[ "Content-Type", "application/json" ] ()
  in
  let rec split_and_send text =
    if String.length text <= max_message_length
    then [ send_message' text ]
    else (
      let prefix = String.prefix text max_message_length in
      match String.rsplit2 prefix ~on:'\n' with
      | Some (text_to_send, rest) ->
        send_message' text_to_send
        :: split_and_send (rest ^ String.drop_prefix text max_message_length)
      | None ->
        send_message' prefix
        :: split_and_send (String.drop_prefix text max_message_length))
  in
  (* should probably introduce some kind of retry logic *)
  Deferred.Result.all (split_and_send text) >>| ignore
;;

let send_message'
  ~chat_id
  ~text
  ?(parse_mode = Some Markdown)
  ?(disable_web_page_preview = false)
  ?(reply_markup = None)
  ()
  =
  Deferred.ignore_m
    (send_message ~chat_id ~text ~parse_mode ~disable_web_page_preview ~reply_markup ())
;;

let send_message_don't_wait
  ~chat_id
  ~text
  ?(parse_mode = Some Markdown)
  ?(disable_web_page_preview = false)
  ?(reply_markup = None)
  ()
  =
  send_message ~chat_id ~text ~parse_mode ~disable_web_page_preview ~reply_markup ()
  |> don't_wait_for
;;

let get_file id =
  let map_body body =
    let result =
      Result.try_with (fun () ->
        body |> Jsonaf.of_string |> Jsonaf.member_exn "result" |> file_of_jsonaf)
    in
    Result.map_error result ~f:(fun err -> `Exn err)
  in
  let uri = uri "getFile" [ "file_id", [ id ] ] in
  let%map result = Http.request' `GET uri () in
  Result.bind result ~f:(fun (_, body) -> map_body body)
;;

let download_file path =
  let uri = uri ~base:"/file/" path [] in
  Http.request `GET uri ()
;;

let multipart_body ~fields ~name ~data ~filename ~mimetype ~boundary =
  let boundary = "--" ^ boundary in
  let ending = boundary ^ "--" in
  let fields =
    fields
    |> List.map ~f:(fun (name, value) ->
         boundary
         ^ "\r\n"
         ^ "Content-Disposition: form-data; name=\""
         ^ name
         ^ "\""
         ^ "\r\n"
         ^ "\r\n"
         ^ value
         ^ "\r\n")
    |> String.concat ~sep:""
  in
  let data =
    boundary
    ^ "\r\n"
    ^ "Content-Disposition: form-data; name=\""
    ^ name
    ^ "\"; filename=\""
    ^ filename
    ^ "\""
    ^ "\r\n"
    ^ "Content-Type: "
    ^ mimetype
    ^ "\r\n"
    ^ "\r\n"
    ^ data
    ^ "\r\n"
  in
  fields ^ data ^ ending
;;

let send_photo ~chat_id ~photo ~filename ~mimetype =
  let uri = uri "sendPhoto" [] in
  let boundary = "--BoundaryFDGigsjIGGJEn" in
  let body =
    multipart_body
      ~fields:[ "chat_id", Int64.to_string chat_id ]
      ~name:"photo"
      ~data:photo
      ~filename
      ~mimetype
      ~boundary
  in
  Http.request
    `POST
    uri
    ~headers:[ "Content-Type", "multipart/form-data; boundary=" ^ boundary ]
    ~body:(Some body)
    ()
;;

(* Utils *)
let parse_update update =
  match update with
  | { message = Some { chat = { id = chat_id; _ }; text = Some t; _ }; _ } ->
    (match String.split t ~on:' ' with
     | command :: args ->
       let command = command |> Caml.String.trim |> String.lowercase in
       let args = args |> List.map ~f:Caml.String.trim in
       `Command (command, args, chat_id, update)
     | _ -> `Unknown)
  | { message = Some { chat = { id = chat_id; _ }; photo = photos; _ }; _ }
    when not (List.is_empty photos) -> `Photos (photos, chat_id, update)
  | { callback_query = Some { message = Some { chat = { id = chat_id; _ }; _ }; data; _ }
    ; _
    } ->
    let sexp = Option.map data ~f:Sexp.of_string in
    `Callback_query (sexp, chat_id, update)
  | _ -> `Unknown
;;
