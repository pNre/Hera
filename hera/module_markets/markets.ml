open Async
open Core

(* Types *)

type market_price =
  { raw : float
  ; fmt : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type market_time =
  { raw : int64
  ; fmt : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type quote =
  { full_exchange_name : string [@key "fullExchangeName"]
  ; symbol : string
  ; quote_type : string [@key "quoteType"]
  ; currency : string
  ; regular_market_price : market_price [@key "regularMarketPrice"]
  ; exchange : string
  ; short_name : string [@key "shortName"]
  ; long_name : string [@key "longName"]
  ; regular_market_time : market_time [@key "regularMarketTime"]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

let quotes_uri symbol =
  let fields =
    [ "longName"
    ; "shortName"
    ; "regularMarketPrice"
    ; "regularMarketChange"
    ; "regularMarketPercent"
    ; "marketCap"
    ; "underlyingSymbol"
    ; "underlyingExchangeSymbol"
    ; "headSymbolAsString"
    ; "underlyingExchangeSymbol"
    ; "headSymbolAsString"
    ; "regularMarketVolume"
    ; "regularMarketOpen"
    ]
  in
  let query =
    [ "formatted", [ "true" ]
    ; "symbols", [ symbol ]
    ; "lang", [ "en-US" ]
    ; "region", [ "US" ]
    ; "fields", [ String.concat ~sep:"," fields ]
    ]
  in
  Uri.make
    ~scheme:"https"
    ~host:"query2.finance.yahoo.com"
    ~path:"/v7/finance/quote"
    ~query
    ()
;;

(* Time series *)
let quotes ~symbol =
  let decode_entities s = s |> String.substr_replace_all ~pattern:"&amp;" ~with_:"&" in
  let map_result body =
    Result.try_with (fun () ->
      body
      |> Jsonaf.of_string
      |> Jsonaf.member_exn "quoteResponse"
      |> Jsonaf.member_exn "result"
      |> Jsonaf.index_exn 0
      |> quote_of_jsonaf)
    |> Result.map ~f:(fun r -> { r with long_name = decode_entities r.long_name })
    |> Result.map_error ~f:(fun err -> `Exn err)
  in
  let%map result = Http.request' `GET (quotes_uri symbol) () in
  Result.bind result ~f:(fun (_, body) -> map_result body)
;;
