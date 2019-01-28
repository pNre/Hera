open Core

type user =
  { id : int64
  ; is_bot : bool
  ; first_name : string
  ; last_name : string option [@default None]
  ; username : string option [@default None]
  ; language_code : string option [@default None] }
[@@deriving of_yojson {strict = false}]

type chat =
  { id : int64
  ; chat_type : string [@key "type"]
  ; title : string option [@default None]
  ; username : string option [@default None]
  ; first_name : string option [@default None]
  ; last_name : string option [@default None]
  ; all_members_are_administrators : bool option [@default None]
  ; description : string option [@default None]
  ; invite_link : string option [@default None] }
[@@deriving of_yojson {strict = false}]

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
  ; user : user option [@default None] }
[@@deriving of_yojson {strict = false}]

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
  ; new_chat_title : string option [@default None] }
[@@deriving of_yojson {strict = false}]

type inline_query =
  { id : int64
  ; from : user
  ; query : string
  ; offset : string }
[@@deriving of_yojson]

type chosen_inline_result =
  { result_id : string
  ; from : user
  ; inline_message_id : string option [@default None]
  ; query : string }
[@@deriving of_yojson]

type callback_query =
  { id : string
  ; from : user
  ; message : message option [@default None]
  ; inline_message_id : string option [@default None]
  ; chat_instance : string
  ; data : string option [@default None] }
[@@deriving of_yojson {strict = false}]

type update =
  { update_id : int64
  ; message : message option [@default None]
  ; edited_message : message option [@default None]
  ; channel_post : message option [@default None]
  ; edited_channel_post : message option [@default None]
  ; inline_query : inline_query option [@default None]
  ; chosen_inline_result : chosen_inline_result option [@default None]
  ; callback_query : callback_query option [@default None] }
[@@deriving of_yojson {strict = false}]

type parse_mode =
  | Markdown
  | HTML

let string_of_parse_mode mode = match mode with Markdown -> "Markdown" | HTML -> "HTML"
let token = Sys.getenv_exn "TELEGRAM_BOT_TOKEN"

let uri path =
  let uri = Uri.of_string "https://api.telegram.org" in
  Uri.with_path uri path
;;

let post path body = Http.request `POST (uri path) [] ~body ()
let get path = Http.request `GET (uri path) [] ()

let set_webhook url =
  let qs = Uri.encoded_of_query ["url", [url]] in
  let path = sprintf "/bot%s/setWebhook?%s" token qs in
  get path
;;

let send_message ~chat_id ~text ?(parse_mode = Some Markdown) () =
  let qs =
    Uri.encoded_of_query
      [ "chat_id", [Int64.to_string chat_id]
      ; "text", [text]
      ; ( "parse_mode"
        , [parse_mode |> Option.map ~f:string_of_parse_mode] |> List.filter_opt ) ]
  in
  let path = sprintf "/bot%s/sendMessage?%s" token qs in
  get path
;;
