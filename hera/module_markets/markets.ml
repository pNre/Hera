open Async
open Core

(* Types *)

type market_price =
  { raw : float
  ; fmt : string
  }
[@@deriving of_yojson { strict = false }]

type market_time =
  { raw : int64
  ; fmt : string
  }
[@@deriving of_yojson { strict = false }]

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
[@@deriving of_yojson { strict = false }]

type error =
  | Http of Http.error
  | Decoding of string

let string_of_error = function
  | Http error -> Http.string_of_error error
  | Decoding error -> error
;;

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
    body
    |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "quoteResponse"
    |> Yojson.Safe.Util.member "result"
    |> Yojson.Safe.Util.index 0
    |> quote_of_yojson
    |> Result.map ~f:(fun r -> { r with long_name = decode_entities r.long_name })
    |> Result.map_error ~f:(fun err -> Decoding err)
  in
  Http.request `GET (quotes_uri symbol) ()
  |> Deferred.Result.map_error ~f:(fun err -> Http err)
  >>=? fun (_, body) -> body |> Http.string_of_body >>| map_result
;;
