open Async
open Core
open Cohttp_async

let failure_handler _socket exn =
  Logging.Main.error "server handler failed with %s" (Exn.to_string exn)
;;

let request_handler ~body _ req =
  match req, Request.uri req |> Uri.path with
  | {Request.meth = `POST; _}, "/" ->
    Body.to_string body
    >>= fun body ->
    Logging.Main.info "%s" body;
    let json = Yojson.Safe.from_string body in
    let update = Telegram.update_of_yojson json in
    (match update with
    | Ok update -> Dispatcher.dispatch update
    | Error err ->
      Logging.Main.error "Decoding -> %s" err;
      Server.respond `Internal_server_error)
  | _ -> Server.respond `Not_found
;;

let main () =
  Dispatcher.register_modules ();
  Telegram.set_webhook (Sys.getenv_exn "TELEGRAM_WEBHOOK_URL")
  >>= (fun _ ->
        Logging.Main.info "Starting webserver";
        Server.create
          ~on_handler_error:(`Call failure_handler)
          (Async_extra.Tcp.Where_to_listen.of_port 8001)
          request_handler )
  >>= fun _ -> Deferred.never ()
;;

let () =
  Async.Command.async_spec ~summary:"command" Command.Spec.empty main |> Command.run
;;
