open Async
open Core
open Httpaf
open Httpaf_async

type http_request =
  { host_and_port : Host_and_port.t
  ; http_method : Method.t
  ; http_headers : (string * string) list
  ; path : string }

type http_error =
  | Request of Client_connection.error
  | Response of Response.t * [`read] Body.t

let read_body body on_body_read =
  let buffer = Bigbuffer.create 1024 in
  let rec read () =
    Body.schedule_read
      body
      ~on_eof:(fun () -> on_body_read buffer)
      ~on_read:(fun response ~off ~len ->
        let body_string = Bigstring.to_string ~off ~len response in
        Bigbuffer.add_string buffer body_string;
        read () )
  in
  read ()
;;

(* Responses *)

let response_headers =
  Headers.of_list ["Content-Type", "text/plain"; "Connection", "close"]
;;

let respond_with_status reqd status =
  let response = Response.create ~headers:response_headers status in
  Reqd.respond_with_string reqd response ""
;;

(* Requests *)

let request_headers host additional_headers =
  let base_headers = ["Connection", "close"; "Host", host] in
  let headers = List.append base_headers additional_headers in
  Headers.of_list headers
;;

let response_handler i response response_body =
  match response with
  | {Response.status = `OK; _} ->
    read_body response_body (fun body -> Ivar.fill i (Ok (response, body)))
  | {Response.status; _} ->
    Ivar.fill i (Error (Response (response, response_body)));
    Log.Global.error "%s" (Status.to_string status)
;;

let request_error i err =
  Log.Global.error "Request failed";
  Ivar.fill i (Error (Request err))
;;

let request req ?(on_write_body = Body.close_writer) () =
  Log.Global.info
    "Requesting %s:%d %s"
    req.host_and_port.host
    req.host_and_port.port
    req.path;
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port req.host_and_port in
  try_with ~extract_exn:true (fun _ ->
      Tcp.connect_sock where_to_connect
      >>= fun socket ->
      let request =
        Request.create
          ~headers:(request_headers req.host_and_port.host req.http_headers)
          req.http_method
          req.path
      in
      Deferred.create (fun i ->
          let request_body =
            Client.SSL.request
              ~error_handler:(request_error i)
              ~response_handler:(response_handler i)
              socket
              request
          in
          on_write_body request_body ) )
  >>| function
  | Ok x -> x
  | Error exn ->
    Log.Global.error "Request failed with exn %s" (Exn.to_string exn);
    Error (Request (`Exn exn))
;;
