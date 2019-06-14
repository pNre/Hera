open Async
open Core
open Markets

let handle_failure chat_id err =
  Logging.Module.error "%s" err;
  let text = sprintf "No results (`%s`)" err in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

let reply chat_id text = Telegram.send_message ~chat_id ~text () |> don't_wait_for

(* Continous updates *)
let load_positions () =
  let start_checking_position position =
    Position.start_checking position (reply (Int64.of_string position.pos.owner_id))
  in
  Position.positions () >>> Result.iter ~f:(List.iter ~f:start_checking_position)
;;

(* Instant price *)
let handle_stock_price_success chat_id quote =
  let text =
    sprintf
      "Last update: %s\nClose: `%s`"
      quote.regular_market_time.fmt
      quote.regular_market_price.fmt
  in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

let get_time_series ~chat_id ~stock =
  Markets.quotes ~symbol:stock
  >>> function
  | Ok quote -> handle_stock_price_success chat_id quote
  | Error (Decoding err) -> handle_failure chat_id ("Invalid response: `" ^ err ^ "`")
  | Error _ -> handle_failure chat_id ("No stocks found for " ^ stock)
;;

(* Bot module *)
let register () = load_positions ()

let help () =
  "*Financial markets*\n\
   `ms [symbol]`\n\
   `mpl` - list open positions\n\
   `mpa [symbol] [price] [size]` - add a position\n\
   `mpd [id]` - deletes a position"
;;

let on_update update =
  match Telegram.parse_update update with
  | `Command ("ms", stock :: _, chat_id, _) ->
    get_time_series ~chat_id ~stock;
    true
  | `Command ("mpl", _, chat_id, _) ->
    Position.list ~owner_id:chat_id ~reply:(reply chat_id);
    true
  | `Command ("mpa", symbol :: price :: size :: _, chat_id, _) ->
    Position.add
      ~owner_id:(Int64.to_string chat_id)
      ~symbol
      ~price
      ~size
      ~reply:(reply chat_id);
    true
  | `Command ("mpd", id :: _, chat_id, _) ->
    Position.delete ~owner_id:(Int64.to_string chat_id) ~id ~reply:(reply chat_id);
    true
  | _ -> false
;;
