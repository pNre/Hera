open Async
open Core

module Dispatcher : Bot.Module.t = struct
  type weather =
    { timestamp : string [@key "ts"]
    ; temperature : int [@key "tp"]
    ; pressure : int [@key "pr"]
    ; humidity : int [@key "hu"]
    ; wind_speed : int [@key "ws"]
    ; wind_direction : int [@key "wd"]
    ; ic : string }
  [@@deriving of_yojson {strict = false}]

  type pollution =
    { timestamp : string [@key "ts"]
    ; aqi : int [@key "aqius"]
    ; main : string [@key "mainus"] }
  [@@deriving of_yojson {strict = false}]

  type current_state =
    { weather : weather
    ; pollution : pollution }
  [@@deriving of_yojson {strict = false}]

  type city =
    { city : string
    ; state : string
    ; country : string
    ; current : current_state }
  [@@deriving of_yojson {strict = false}]

  type response =
    { status : string
    ; data : city }
  [@@deriving of_yojson {strict = false}]

  let api_key = Sys.getenv_exn "AIRVISUAL_API_KEY"
  let host_and_port = Host_and_port.{host = "api.airvisual.com"; port = 443}

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

  let handle_failure chat_id err =
    Log.Global.error "%s" err;
    let text = sprintf "No results (`%s`)" err in
    don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
  ;;

  let handle_success chat_id body =
    Log.Global.info "%s" (Bigbuffer.contents body);
    let result =
      body |> Bigbuffer.contents |> Yojson.Safe.from_string |> response_of_yojson
    in
    match result with
    | Ok {status = _; data = city} ->
      let main = city.current.pollution.main in
      let aqi = city.current.pollution.aqi in
      let temperature = city.current.weather.temperature in
      let humidity = city.current.weather.humidity in
      let text =
        sprintf
          "Air quality in %s, %s\n\
           Main pollutant: *%s* (_%d_, %s)\n\
           Temperature: %d°\n\
           Humidity: %d%%"
          city.city
          city.country
          (description_of_concern main)
          aqi
          (string_of_aqi aqi)
          temperature
          humidity
      in
      don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
    | Error err -> handle_failure chat_id err
  ;;

  let get_air_quality ~chat_id ~city ~state ~country =
    let params =
      [ "key", [Uri.pct_encode api_key]
      ; "city", [Uri.pct_encode city]
      ; "state", [Uri.pct_encode state]
      ; "country", [Uri.pct_encode country] ]
    in
    let qs = Uri.encoded_of_query params in
    let path = "/v2/city?" ^ qs in
    let req = Http.{host_and_port; http_method = `GET; http_headers = []; path} in
    let res = Http.request req () in
    res
    >>> function
    | Ok (_, body) -> handle_success chat_id body | Error _ -> handle_failure chat_id "/"
  ;;

  let resolve_known_city = function
    | "milano" | "milan" -> Some ("milano", "lombardy", "italy")
    | "londra" | "london" -> Some ("london", "england", "uk")
    | _ -> None
  ;;

  let location_of_query q =
    let params =
      q
      |> String.chop_prefix_exn ~prefix:"aq "
      |> String.split ~on:','
      |> List.map ~f:Caml.String.trim
      |> List.map ~f:String.lowercase
    in
    match params with
    | city :: state :: country :: _ -> Some (city, state, country)
    | [known_city] -> resolve_known_city known_city
    | _ -> None
  ;;

  (* Bot module *)
  let register () = ()
  let help () = "*Air quality*\n`aq [city], [state], [country]`"

  let on_update _reqd update =
    match update with
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.is_prefix t ~prefix:"aq " ->
      (match location_of_query t with
      | Some (city, state, country) ->
        get_air_quality ~chat_id ~city ~state ~country;
        true
      | _ -> false)
    | _ -> false
  ;;
end
