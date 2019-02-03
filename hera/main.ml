open Async
open Core
open Cohttp_async

let failure_handler _socket exn =
  Logging.Main.error "server handler failed with %s" (Exn.to_string exn)
;;

let request_handler ~body _ req =
  match req, Request.uri req |> Uri.path with
  | {Request.meth = `POST; _}, path when path = "/" ^ Telegram.token ->
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

let main port () =
  Dispatcher.register_modules ();
  Sys.getenv_exn "TELEGRAM_WEBHOOK_URL"
  |> Uri.of_string
  |> fun uri ->
  Uri.with_path uri ("/" ^ Telegram.token)
  |> Telegram.set_webhook
  >>= (fun _ ->
        Logging.Main.info "Starting webserver";
        Server.create
          ~on_handler_error:(`Call failure_handler)
          (Async_extra.Tcp.Where_to_listen.of_port port)
          request_handler )
  >>= fun _ -> Deferred.never ()
;;

let () =
  let open Async.Command.Let_syntax in
  Async.Command.async
    ~summary:"Telegram bot that does stuff"
    [%map_open
      let port = flag "-p" (optional_with_default 8001 int) ~doc:"port to listen on" in
      main port]
  |> Async.Command.run
;;
