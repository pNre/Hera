open Async
open Core

module Dispatcher: Bot_module.Module.t = struct
  type symbol = {
    symbol: string;
    name: string;
  }
  [@@deriving of_yojson { strict = false }]

  let symbols =
    ref []

  let host_and_port =
    Host_and_port.{ host = "api.iextrading.com"; port = 443 }

  let handle_failure chat_id err =
    Log.Global.error "%s" err;
    let text = sprintf "No results (`%s`)" err in
    don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)

  (* Stock price *)
  let handle_stock_price_success chat_id body =
    let value = Bigbuffer.contents body in
    let text = sprintf "`%s`" value in
    don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)

  let stock_in_symbols predicate =
    let rec _stock_in_symbols symbols =
      match symbols with
      | [] -> None
      | stock :: _ when predicate stock -> Some stock
      | _ :: rest -> _stock_in_symbols rest in
    _stock_in_symbols !symbols

  let stock_of_symbol symbol =
    stock_in_symbols (fun s -> s.symbol = symbol)

  let stock_of_name name =
    stock_in_symbols (fun s -> String.Caseless.equal s.name name)

  let get_stock_price ~chat_id ~stock =
    match Option.first_some (stock_of_name stock) (stock_of_symbol stock) with
    | Some { symbol; _ } ->
      let path =
        symbol
        |> String.lowercase
        |> Uri.pct_encode
        |> sprintf "/1.0/stock/%s/price" in
      let req = Http.{ host_and_port; http_method = `GET; http_headers = []; path } in
      let res = Http.request req () in
      Deferred.upon res (function
        | Ok (_, body) -> handle_stock_price_success chat_id body
        | Error _ -> handle_failure chat_id "/")
    | None ->
      handle_failure chat_id ("No stocks found for " ^ stock)

  (* Symbols *)
  let handle_symbols_success body =
    let response =
      body
      |> Bigbuffer.contents
      |> Yojson.Safe.from_string in
    match response with
    | `List syms ->
      symbols :=
        syms
        |> List.map ~f:symbol_of_yojson
        |> List.filter_map ~f:Result.ok
    | _ -> ()

  let get_symbols () =
    let req = Http.{ host_and_port; http_method = `GET; http_headers = []; path = "/1.0/ref-data/symbols" } in
    let res = Http.request req () in
    Deferred.upon res (function
        | Ok (_, body) -> handle_symbols_success body
        | Error _ -> ())

  (* Bot module *)
  let register () =
    get_symbols ()

  let help () =
    "*Stocks price*\n`s [symbol]`\n`s [company name]`"

  let on_update _reqd update =
    match update with
    | { Telegram.message = Some { chat = { id = chat_id; _ }; text = Some t; _ }; _ } when String.is_prefix t ~prefix:"s " ->
      let stock = String.chop_prefix_exn t ~prefix:"s " in
      get_stock_price ~chat_id ~stock;
      true
    | _ ->
      false
end
