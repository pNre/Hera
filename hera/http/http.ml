open Async
open Core
open Httpaf
open Httpaf_async

type http_request =
  { http_method : Method.t
  ; scheme : string
  ; host : string
  ; path : string
  ; http_headers : (string * string) list }

type http_error =
  | Format
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

let request_headers req =
  let base_headers = ["Connection", "close"; "Host", req.host] in
  let headers = List.append base_headers req.http_headers in
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

let host_and_port_create scheme host =
  let port = match scheme with "https" -> 443 | _ -> 80 in
  Host_and_port.create ~host ~port
;;

let update_request_for_redirect req response_headers =
  let location = Uri.of_string (Headers.get_exn response_headers "location") in
  let path = Uri.path location in
  match Uri.host location with
  | Some host ->
    let scheme = Uri.scheme location |> Option.value ~default:"https" in
    Some {req with scheme; host; path}
  | _ -> None
;;

let rec perform_request req on_write_body max_redirects i =
  let response_handler i response response_body =
    match response with
    | {Response.status = `OK; _} ->
      read_body response_body (fun body -> Ivar.fill i (Ok (response, body)))
    | {Response.status = `Found; headers; _}
    | {Response.status = `Moved_permanently; headers; _}
    | {Response.status = `Temporary_redirect; headers; _}
      when Headers.mem headers "location" && max_redirects > 0 ->
      (match update_request_for_redirect req headers with
      | Some nreq ->
        Log.Global.info
          "Redirecting from %s%s to %s%s (%d redirects left)"
          req.host
          req.path
          nreq.host
          nreq.path
          max_redirects;
        perform_request nreq on_write_body (max_redirects - 1) i
      | None -> failwith "fixme")
    | {Response.status; _} ->
      Ivar.fill i (Error (Response (response, response_body)));
      Log.Global.error "%s, max_redirects = %d" (Status.to_string status) max_redirects
  in
  let request = Request.create ~headers:(request_headers req) req.http_method req.path in
  try_with ~extract_exn:true (fun _ ->
      let host_and_port = host_and_port_create req.scheme req.host in
      let where_to_connect = Tcp.Where_to_connect.of_host_and_port host_and_port in
      Tcp.connect_sock where_to_connect
      >>| fun socket ->
      let request_body =
        match host_and_port.port with
        | 80 ->
          Client.request
            ~error_handler:(request_error i)
            ~response_handler:(response_handler i)
            socket
            request
        | _ ->
          Client.SSL.request
            ~error_handler:(request_error i)
            ~response_handler:(response_handler i)
            socket
            request
      in
      on_write_body request_body )
  >>> function Ok _ -> () | Error exn -> Ivar.fill i (Error (Request (`Exn exn)))
;;

let request
    http_method
    uri
    http_headers
    ?(on_write_body = Body.close_writer)
    ?(max_redirects = 3)
    () =
  let path = Uri.path uri in
  match Uri.host uri with
  | Some host ->
    let scheme = uri |> Uri.scheme |> Option.value ~default:"https" in
    let req = {http_method; scheme; host; path; http_headers} in
    Log.Global.info "Requesting %s" (Uri.to_string uri);
    Deferred.create (perform_request req on_write_body max_redirects)
  | _ -> Deferred.return (Result.fail Format)
;;
