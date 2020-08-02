open Async
open Core
open Poly

type user =
  { id : int64
  ; is_bot : bool
  ; first_name : string
  ; last_name : string option [@default None]
  ; username : string option [@default None]
  ; language_code : string option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type chat =
  { id : int64
  ; chat_type : string [@key "type"]
  ; title : string option [@default None]
  ; username : string option [@default None]
  ; first_name : string option [@default None]
  ; last_name : string option [@default None]
  ; all_members_are_administrators : bool option [@default None]
  ; description : string option [@default None]
  ; invite_link : string option [@default None]
  }
[@@deriving of_yojson { strict = false }]

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
[@@deriving of_yojson]

type message_entity =
  { entity_type : string [@key "type"]
  ; offset : int
  ; length : int
  ; url : string option [@default None]
  ; user : user option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type photo_size =
  { file_id : string
  ; width : int
  ; height : int
  ; file_size : int option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type message =
  { message_id : int64
  ; from : user
  ; date : int64
  ; chat : chat
  ; forward_from : user option [@default None]
  ; forward_from_chat : chat option [@default None]
  ; forward_from_message_id : int option [@default None]
  ; forward_signature : string option [@default None]
  ; forward_date : int option [@default None]
  ; edit_date : int64 option [@default None]
  ; media_group_id : string option [@default None]
  ; author_signature : string option [@default None]
  ; text : string option [@default None]
  ; entities : message_entity list [@default []]
  ; caption_entities : message_entity list [@default []]
  ; caption : string option [@default None]
  ; new_chat_members : user list [@default []]
  ; left_chat_member : user option [@default None]
  ; new_chat_title : string option [@default None]
  ; photo : photo_size list [@default []]
  }
[@@deriving of_yojson { strict = false }]

type inline_query =
  { id : int64
  ; from : user
  ; query : string
  ; offset : string
  }
[@@deriving of_yojson]

type chosen_inline_result =
  { result_id : string
  ; from : user
  ; inline_message_id : string option [@default None]
  ; query : string
  }
[@@deriving of_yojson]

type callback_query =
  { id : string
  ; from : user
  ; message : message option [@default None]
  ; inline_message_id : string option [@default None]
  ; chat_instance : string
  ; data : string option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type update =
  { update_id : int64
  ; message : message option [@default None]
  ; edited_message : message option [@default None]
  ; channel_post : message option [@default None]
  ; edited_channel_post : message option [@default None]
  ; inline_query : inline_query option [@default None]
  ; chosen_inline_result : chosen_inline_result option [@default None]
  ; callback_query : callback_query option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type file =
  { file_id : string
  ; file_size : int option [@default None]
  ; file_path : string option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type inline_keyboard_button =
  { text : string
  ; url : string option [@default None]
  ; callback_data : string option [@default None]
  }
[@@deriving make, to_yojson { strict = false }]

type inline_keyboard_markup = { inline_keyboard : inline_keyboard_button list list }
[@@deriving make, to_yojson { strict = false }]

type send_message =
  { chat_id : string
  ; text : string
  ; parse_mode : string option [@default None]
  ; disable_web_page_preview : bool [@default false]
  ; reply_markup : Yojson.Safe.t option [@default None]
  }
[@@deriving make, to_yojson { strict = false }]

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
      |> send_message_to_yojson
      |> Yojson.Safe.to_string
      |> Option.return
    in
    Logging.Main.info "%s" (Option.value_exn body);
    let uri = uri "sendMessage" [] in
    Http.request `POST uri ~body ~http_headers:[ "Content-Type", "application/json" ] ()
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

let get_file id =
  let map_body body =
    let json = body |> Yojson.Safe.from_string in
    try json |> Yojson.Safe.Util.member "result" |> file_of_yojson with
    | _ -> Error "Invalid response"
  in
  let uri = uri "getFile" [ "file_id", [ id ] ] in
  Http.request `GET uri ()
  >>=? fun (response, body) ->
  Http.string_of_body body
  >>| map_body
  >>| Result.map_error ~f:(fun _ -> Http.Response (response, body))
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
    ~http_headers:[ "Content-Type", "multipart/form-data; boundary=" ^ boundary ]
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
