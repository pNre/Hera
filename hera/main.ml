open Async
open Core
open Httpaf
open Httpaf_async

let error_handler _ ?request:_ _error start_response =
  let response_body = start_response Headers.empty in
  Body.write_string response_body "\n";
  Body.close_writer response_body

let request_handler _ reqd =
  let request_body = Reqd.request_body reqd in
  match Reqd.request reqd with
  | { Request.meth = `POST; target = "/"; _ } ->
    Http.read_body request_body
      (fun body ->
         let body_string = Bigbuffer.contents body in
         Log.Global.info "%s" body_string;
         let json = Yojson.Safe.from_string body_string in
         let update = Telegram.update_of_yojson json in
         match update with
         | Ok update ->
           Dispatcher.dispatch reqd update
         | Error err ->
           Log.Global.error "Decoding -> %s" err;
           Http.respond_with_status reqd `Internal_server_error)
  | _ ->
    Http.respond_with_status reqd `Not_found

let main () =
  Dispatcher.register_modules ();
  Telegram.set_webhook (Sys.getenv_exn "WEBHOOK_URL")
  >>= (fun _ ->
      let where_to_listen = Tcp.Where_to_listen.of_port 8001 in
      Tcp.Server.create_sock ~on_handler_error:`Raise
        where_to_listen
        (Server.create_connection_handler ~request_handler ~error_handler))
  >>= fun _ -> Deferred.never ()

let () =
  Async.Command.async_spec ~summary:"command"
    Command.Spec.empty
    main
  |> Command.run
