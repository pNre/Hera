open Async
open Core
open Cohttp
open Cohttp_async
open Logging.Http

type error =
  [ `Exn of exn
  | `Http_response_error of int * Body.t
  ]

exception Too_many_redirects
exception Invalid_redirection

let string_of_method = Code.string_of_method
let string_of_body = Body.to_string
let pipe_of_body = Body.to_pipe

let call ~headers ~body meth uri =
  let call () =
    Client.call
      ~headers:(Header.of_list headers)
      ~body:(Option.value_map body ~default:`Empty ~f:Body.of_string)
      meth
      uri
  in
  try_with ~extract_exn:true call |> Deferred.Result.map_error ~f:(fun exn -> `Exn exn)
;;

let request ?(headers = []) ?(max_redirects = 3) ?(body = None) meth uri () =
  let open Deferred.Result in
  let open Let_syntax in
  let rec perform uri redirects_left =
    match%bind call ~headers ~body meth uri with
    | ({ status = #Code.success_status; _ } as response), body ->
      debug !"← %{Code.string_of_method} %{Uri}" meth uri;
      return (response, body)
    | { status = #Code.redirection_status; headers; _ }, _ ->
      (match Header.get_location headers with
       | Some redirect_uri when redirects_left > 0 ->
         info
           !"← Redirected from %{Code.string_of_method} %{Uri} to %{Uri}"
           meth
           uri
           redirect_uri;
         perform redirect_uri (redirects_left - 1)
       | Some _ -> fail (`Exn Too_many_redirects)
       | None -> fail (`Exn Invalid_redirection))
    | response, body ->
      error !"%{Code.string_of_method} %{Uri} %{sexp:Cohttp.Response.t}" meth uri response;
      fail (`Http_response_error (Cohttp.Code.code_of_status response.status, body))
  in
  debug !"→ %{Code.string_of_method} %{Uri} %s" meth uri (Option.value body ~default:"");
  perform uri max_redirects
;;

let request' ?(headers = []) ?(max_redirects = 3) ?(body = None) meth uri () =
  request ~headers ~max_redirects ~body meth uri ()
  |> Deferred.Result.bind ~f:(fun (response, body) ->
       let%map body = string_of_body body in
       Result.return (response, body))
;;
