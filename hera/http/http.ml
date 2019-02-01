open Async
open Core
open Cohttp
open Cohttp_async

type http_error =
  | Format
  | Request of exn
  | Response of Response.t * Body.t

let string_of_body = Cohttp_async.Body.to_string
let pipe_of_body = Cohttp_async.Body.to_pipe

let request http_method uri ?(http_headers = []) ?(body = None) ?(max_redirects = 3) () =
  let rec perform uri redirects_left =
    if redirects_left = 0
    then Deferred.return (Error (Request (Failure "Too many redirects")))
    else
      let call =
        try_with (fun _ ->
            Client.call
              ~headers:(Header.of_list http_headers)
              ~body:(Option.value_map body ~default:`Empty ~f:Body.of_string)
              http_method
              uri )
      in
      call
      >>| Result.map_error ~f:(fun e -> Request e)
      >>=? function
      | ({status; _} as response), body when Code.is_success (Code.code_of_status status)
      -> (response, body) |> Result.return |> Deferred.return
      | ({status; headers; _} as response), body
        when Code.is_redirection (Code.code_of_status status) ->
        Logging.Http.info
          "Redirecting %s to %s"
          (Uri.to_string uri)
          (Header.get_location headers |> Option.value_map ~f:Uri.to_string ~default:"?");
        (match Header.get_location headers with
        | Some redirect_uri -> perform redirect_uri (redirects_left - 1)
        | None -> Response (response, body) |> Result.fail |> Deferred.return)
      | response, body -> Response (response, body) |> Result.fail |> Deferred.return
  in
  Logging.Http.debug "Requesting %s" (Uri.to_string uri);
  perform uri max_redirects
;;
