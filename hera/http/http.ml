open Async
open Core
open Httpaf
open Httpaf_async

type http_request =
  { http_method : Method.t
  ; scheme : string
  ; host : string
  ; path : string
  ; query : (string * string list) list
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
  let base_headers =
    [ "Connection", "close"
    ; "Host", req.host
    ; "Accept", "*/*"
    ; "User-Agent", "hera/1.0" ]
  in
  let headers = List.append base_headers req.http_headers in
  Headers.of_list headers
;;

let response_handler i response response_body =
  match response with
  | {Response.status = `OK; _} ->
    read_body response_body (fun body -> Ivar.fill i (Ok (response, body)))
  | {Response.status; _} ->
    Ivar.fill i (Error (Response (response, response_body)));
    Logging.Http.error "%s" (Status.to_string status)
;;

let request_error req i err =
  let request_uri =
    Uri.make ~scheme:req.scheme ~host:req.host ~path:req.path ~query:req.query ()
  in
  Logging.Http.error "(%s) request failed" (Uri.to_string request_uri);
  Ivar.fill i (Error (Request err))
;;

let host_and_port_create scheme host =
  let port = match scheme with "https" -> 443 | _ -> 80 in
  Host_and_port.create ~host ~port
;;

let update_request_for_redirect req response_headers =
  let location = Uri.of_string (Headers.get_exn response_headers "location") in
  let path = Uri.path location in
  let query = Uri.query location in
  match Uri.host location with
  | Some host ->
    let scheme = Uri.scheme location |> Option.value ~default:"https" in
    Some {req with scheme; host; path; query}
  | _ -> None
;;

let rec perform_request req body max_redirects i =
  let request_uri =
    Uri.make ~scheme:req.scheme ~host:req.host ~path:req.path ~query:req.query ()
  in
  let response_handler i response response_body =
    match response with
    | {Response.status = `OK; _} ->
      Logging.Http.info "(%s) ok" (Uri.to_string request_uri);
      read_body response_body (fun body -> Ivar.fill i (Ok (response, body)))
    | {Response.status = `Found; headers; _}
    | {Response.status = `Moved_permanently; headers; _}
    | {Response.status = `Temporary_redirect; headers; _}
      when Headers.mem headers "location" && max_redirects > 0 ->
      (match update_request_for_redirect req headers with
      | Some nreq ->
        Logging.Http.info
          "(%s) redirecting to %s://%s%s (%d redirects left)"
          (Uri.to_string request_uri)
          nreq.scheme
          nreq.host
          nreq.path
          max_redirects;
        perform_request nreq body (max_redirects - 1) i
      | None ->
        Ivar.fill i (Error (Response (response, response_body)));
        Logging.Http.error
          "(%s) %s, invalid location %s"
          (Uri.to_string request_uri)
          (Status.to_string response.status)
          (Headers.get_exn response_headers "location"))
    | {Response.status; _} ->
      Ivar.fill i (Error (Response (response, response_body)));
      Logging.Http.error
        "(%s) %s, max_redirects = %d"
        (Uri.to_string request_uri)
        (Status.to_string status)
        max_redirects
  in
  let query_string = Uri.encoded_of_query ~scheme:req.scheme req.query in
  let path =
    if String.length query_string > 0 then req.path ^ "?" ^ query_string else req.path
  in
  let headers = request_headers req in
  let request = Request.create ~headers req.http_method path in
  try_with ~extract_exn:true (fun _ ->
      let host_and_port = host_and_port_create req.scheme req.host in
      let where_to_connect = Tcp.Where_to_connect.of_host_and_port host_and_port in
      Tcp.connect_sock where_to_connect
      >>| fun socket ->
      let request_body =
        match host_and_port.port with
        | 80 ->
          Client.request
            ~error_handler:(request_error req i)
            ~response_handler:(response_handler i)
            socket
            request
        | _ ->
          Client.SSL.request
            ~error_handler:(request_error req i)
            ~response_handler:(response_handler i)
            socket
            request
      in
      Option.iter body ~f:(Body.write_string request_body);
      Body.close_writer request_body )
  >>> function Ok _ -> () | Error exn -> Ivar.fill i (Error (Request (`Exn exn)))
;;

let request http_method uri ?(http_headers = []) ?(body = None) ?(max_redirects = 3) () =
  let path = Uri.path uri in
  let query = Uri.query uri in
  match Uri.host uri with
  | Some host ->
    let scheme = uri |> Uri.scheme |> Option.value ~default:"https" in
    let req = {http_method; scheme; host; path; query; http_headers} in
    Logging.Http.debug "(%s) requesting" (Uri.to_string uri);
    Deferred.create (perform_request req body max_redirects)
  | _ -> Deferred.return (Result.fail Format)
;;
