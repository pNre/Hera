open Core

let string_of_error = function
  | `Exn exn -> Exn.to_string exn
  | `Http_response_error (code, _) -> sprintf "Http error %d" code
;;

let handle_module_error chat_id err =
  let error_message = string_of_error err in
  Logging.Module.error "%s" error_message;
  let text = sprintf "No results (`%s`)" error_message in
  Telegram.send_message_don't_wait ~chat_id ~text ()
;;
