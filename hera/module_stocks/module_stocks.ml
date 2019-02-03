open Async
open Core

type symbol =
  { symbol : string
  ; name : string }
[@@deriving of_yojson {strict = false}]

let symbols = ref []
let uri path = Uri.make ~scheme:"https" ~host:"api.iextrading.com" ~path ()

let handle_failure chat_id err =
  Logging.Module.error "%s" err;
  let text = sprintf "No results (`%s`)" err in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

(* Stock price *)
let handle_stock_price_success chat_id body =
  let text = sprintf "`%s`" body in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

let stock_in_symbols predicate =
  let rec stock_in_symbols' symbols =
    match symbols with
    | [] -> None
    | stock :: _ when predicate stock -> Some stock
    | _ :: rest -> stock_in_symbols' rest
  in
  stock_in_symbols' !symbols
;;

let stock_of_symbol symbol = stock_in_symbols (fun s -> s.symbol = symbol)
let stock_of_name name = stock_in_symbols (fun s -> String.Caseless.equal s.name name)

let get_stock_price ~chat_id ~stock =
  let symbol =
    Option.first_some (stock_of_name stock) (stock_of_symbol stock)
    |> Option.map ~f:(fun {symbol; _} -> symbol)
    |> Option.value ~default:stock
    |> String.lowercase
  in
  let path = symbol |> Uri.pct_encode |> sprintf "/1.0/stock/%s/price" in
  Http.request `GET (uri path) ()
  >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
  >>> function
  | Ok body -> handle_stock_price_success chat_id body
  | Error _ -> handle_failure chat_id ("No stocks found for " ^ stock)
;;

(* Symbols *)
let handle_symbols_success body =
  try
    let response = body |> Yojson.Safe.from_string in
    match response with
    | `List syms ->
      symbols := syms |> List.map ~f:symbol_of_yojson |> List.filter_map ~f:Result.ok
    | _ -> ()
  with _ -> ()
;;

let get_symbols () =
  Http.request `GET (uri "/1.0/ref-data/symbols") ()
  >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
  >>> function Ok body -> handle_symbols_success body | Error _ -> ()
;;

(* Bot module *)
let register () = get_symbols ()
let help () = "*Stocks price*\n`s [symbol]`\n`s [company name]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("s", stock :: _, chat_id, _) ->
    get_stock_price ~chat_id ~stock;
    true
  | _ -> false
;;
