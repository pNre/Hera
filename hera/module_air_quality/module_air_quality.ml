open Async
open Core
open Error_handler
open Jsonaf.Export

type city = { name : string } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type time =
  { iso : string
  ; s : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type vval = { v : float } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type iaqi =
  { o3 : vval option [@jsonaf.option]
  ; pm25 : vval option [@jsonaf.option]
  ; pm10 : vval option [@jsonaf.option]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type data =
  { city : city
  ; dominentpol : string
  ; aqi : int
  ; iaqi : iaqi
  ; time : time
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type response =
  { status : string
  ; data : data
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

let api_token = lazy (Sys.getenv_exn "AQI_TOKEN")

let uri path =
  let token = Lazy.force api_token in
  Uri.make ~scheme:"https" ~host:"api.waqi.info" ~path ~query:[ "token", [ token ] ] ()
;;

let string_of_aqi = function
  | x when x <= 50 -> "Good"
  | x when x <= 100 -> "Moderate"
  | x when x <= 150 -> "Unhealthy-ish"
  | x when x <= 200 -> "*Unhealthy*"
  | x when x <= 300 -> "*Very Unhealthy*"
  | _ -> "*Hazardous, RUN*"
;;

let description_of_concern = function
  | "p2" -> "pm2.5"
  | "p1" -> "pm10"
  | "o3" -> "Ozone, O3"
  | "n2" -> "Nitrogen dioxide, NO2"
  | "s2" -> "Sulfur dioxide, SO2"
  | "co" -> "Carbon monoxide, CO"
  | x -> x
;;

let formatted_vval v = sprintf "%f" v.v

let handle_success chat_id body =
  let result =
    Result.try_with (fun () -> body |> Jsonaf.of_string |> response_of_jsonaf)
  in
  match result with
  | Ok { status = _; data } ->
    let main = data.dominentpol in
    let aqi = data.aqi in
    let ts = data.time.s in
    let pm10 = Option.value_map data.iaqi.pm10 ~default:"-" ~f:formatted_vval in
    let pm25 = Option.value_map data.iaqi.pm25 ~default:"-" ~f:formatted_vval in
    let text =
      sprintf
        "Air quality in %s\n\
         AQI: *%d*, *%s*\n\
         Main pollutant: *%s*\n\
         PM10: *%s*\n\
         PM2.5: *%s*\n\n\
         _Updated on %s_"
        data.city.name
        aqi
        (string_of_aqi aqi)
        (description_of_concern main)
        pm10
        pm25
        ts
    in
    Telegram.send_message_don't_wait ~chat_id ~text ()
  | Error err -> handle_module_error chat_id (`Exn err)
;;

let get_air_quality ~chat_id ~city =
  Http.request `GET (uri ("/feed/" ^ Uri.pct_encode ~component:`Path city)) ()
  >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
  >>> function
  | Ok body -> handle_success chat_id body
  | Error err -> handle_module_error chat_id err
;;

(* Bot module *)
let register () = ()
let help () = "*Air quality*\n`aq [city] [state] [country]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("aq", [ city ], chat_id, _) ->
    get_air_quality ~chat_id ~city;
    true
  | _ -> false
;;
