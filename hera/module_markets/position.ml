open Async
open Core
open Types

let position_tasks = Int.Table.create ~growth_allowed:true ()

let map_market_position pos =
  { pos; price = Bignum.of_string pos.price; size = Bignum.of_string pos.size }
;;

let map_market_positions positions = positions |> List.map ~f:map_market_position
let positions () = Db.market_positions () >>|? map_market_positions

let positions_for_owner owner_id =
  Db.market_positions_for_owner owner_id >>|? map_market_positions
;;

let start_checking position send =
  let check_last_price () =
    let last_time = Mvar.create () in
    let is_past_last_time time =
      last_time
      |> Mvar.peek
      |> Option.value_map ~default:true ~f:(fun last_time -> last_time < time)
    in
    let profit_in_quote price =
      Bignum.(
        let initial_amount = position.price * position.size in
        let current_amount = price * position.size in
        let amount_diff = current_amount - initial_amount in
        to_string_hum ~delimiter:'.' ~decimals:2 amount_diff)
    in
    let change_in_quote close =
      Bignum.(
        let ratio = close / position.price in
        let change = (ratio - one) * hundred in
        to_string_hum ~delimiter:'.' ~decimals:4 change ^ "%")
    in
    let market_time time =
      Time.(
        let time =
          time.Markets.raw
          |> Int63.of_int64_trunc
          |> Span.of_int63_seconds
          |> Time.of_span_since_epoch
        in
        let zone =
          try force Zone.local with
          | _ -> Zone.utc
        in
        let formatted_zone = Zone.abbreviation zone time in
        Time.format time "%d/%m/%Y %H:%M:%S" ~zone ^ " " ^ formatted_zone)
    in
    let format quote =
      Markets.(
        let price = Bignum.of_string quote.regular_market_price.fmt in
        sprintf
          "🎢 _%s_\n*Last update*: %s\n*Close*: `%s`\n*Change*: `%s`\n*Profit*: `%s`"
          position.pos.symbol
          (market_time quote.regular_market_time)
          (quote.regular_market_price.fmt ^ " " ^ quote.currency)
          (change_in_quote price)
          (profit_in_quote price ^ " " ^ quote.currency))
    in
    fun () ->
      Markets.quotes ~symbol:position.pos.symbol
      >>= fun result ->
      Result.iter result ~f:(fun quote ->
          if is_past_last_time quote.regular_market_time.raw
          then (
            Mvar.set last_time quote.regular_market_time.raw;
            format quote |> send)
          else ());
      Deferred.unit
  in
  let cancellation = Ivar.create () in
  Hashtbl.set position_tasks ~key:position.pos.id ~data:cancellation;
  let timespan = Time.Span.create ~min:10 () in
  Clock.every'
    ~stop:(Ivar.read cancellation)
    ~continue_on_error:true
    timespan
    (check_last_price ())
;;

let stop_checking id =
  id |> Hashtbl.find_and_remove position_tasks |> Option.iter ~f:(fun i -> Ivar.fill i ())
;;

let list ~owner_id ~reply =
  let send positions =
    if not (List.is_empty positions)
    then
      positions
      |> List.iter ~f:(fun position ->
             let price = Bignum.to_string_accurate position.price in
             let size = Bignum.to_string_accurate position.size in
             let total =
               Bignum.(
                 to_string_hum ~delimiter:'.' ~decimals:4 (position.price * position.size))
             in
             let text =
               sprintf
                 "_ID:_ %d\n*Symbol*: `%s`\n*Price*: `%s`\n*Size*: `%s`\n*Total*: `%s`"
                 position.pos.id
                 position.pos.symbol
                 (price ^ " " ^ position.pos.currency)
                 size
                 (total ^ " " ^ position.pos.currency)
             in
             reply text)
    else reply "No positions"
  in
  positions_for_owner (Int64.to_string owner_id) >>> Result.iter ~f:send
;;

let add ~owner_id ~symbol ~price ~size ~reply =
  let quotes () =
    Markets.quotes ~symbol >>| Result.map_error ~f:(fun error -> `Market error)
  in
  let insert_position quotes =
    Db.insert_market_position
      ~owner_id
      ~symbol
      ~price
      ~size
      ~currency:quotes.Markets.currency
    >>=? (fun _ -> Db.find_market_position ~owner_id ~symbol)
    >>| Result.map ~f:(fun pos -> pos, quotes)
    >>| Result.map_error ~f:(fun error -> `Db error)
  in
  let start_checking (position, quotes) =
    position
    |> Option.map ~f:map_market_position
    |> Option.iter ~f:(fun position -> start_checking position reply);
    quotes
  in
  try
    let _ = Bignum.of_string price in
    let _ = Bignum.of_string size in
    quotes ()
    >>=? insert_position
    >>|? start_checking
    >>> function
    | Ok quote ->
      let text =
        sprintf
          "👍\nType: *%s*\nExchange: *%s*\nName: *%s*"
          quote.quote_type
          quote.full_exchange_name
          quote.long_name
      in
      reply text
    | Error error ->
      let error_string =
        match error with
        | `Db e -> Db.string_of_error e
        | `Market e -> Markets.string_of_error e
      in
      let text = sprintf "Couldn't add position: %s" error_string in
      reply text
  with
  | _ -> reply "Invalid price or size"
;;

let delete ~owner_id ~id ~reply =
  let int_of_id = id |> int_of_string_opt |> Result.of_option ~error:`Invalid_id in
  let delete_by_id id =
    stop_checking id;
    Db.delete_market_position ~owner_id ~id >>| Result.map_error ~f:(fun e -> `Db e)
  in
  int_of_id
  |> return
  >>=? delete_by_id
  >>> function
  | Ok _ -> reply "Deleted"
  | Error `Invalid_id -> reply "Invalid id"
  | Error (`Db e) ->
    let error_string = Db.string_of_error e in
    let text = sprintf "Couldn't delete position: %s" error_string in
    reply text
;;
