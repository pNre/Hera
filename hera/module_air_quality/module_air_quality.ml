open Async
open Core

type weather =
  { timestamp : string [@key "ts"]
  ; temperature : float [@key "tp"]
  ; pressure : float [@key "pr"]
  ; humidity : float [@key "hu"]
  ; wind_speed : float option [@key "ws"] [@default None]
  ; wind_direction : float option [@key "wd"] [@default None]
  ; ic : string option [@default None] }
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
let uri path query = Uri.make ~scheme:"https" ~host:"api.airvisual.com" ~path ~query ()

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
  Logging.Module.error "%s" err;
  let text = sprintf "No results (`%s`)" err in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

let handle_success chat_id body =
  let result = body |> Yojson.Safe.from_string |> response_of_yojson in
  match result with
  | Ok {status = _; data = city} ->
    let main = city.current.pollution.main in
    let aqi = city.current.pollution.aqi in
    let temperature =
      city.current.weather.temperature |> Float.round_nearest |> Float.to_int
    in
    let humidity =
      city.current.weather.humidity |> Float.round_nearest |> Float.to_int
    in
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
  let query =
    [ "key", [Uri.pct_encode api_key]
    ; "city", [Uri.pct_encode city]
    ; "state", [Uri.pct_encode state]
    ; "country", [Uri.pct_encode country] ]
  in
  Http.request `GET (uri "/v2/city" query) ()
  >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
  >>> function
  | Ok body -> handle_success chat_id body | Error _ -> handle_failure chat_id "/"
;;

let resolve_known_city = function
  | "milano" | "milan" -> Some ("milano", "lombardy", "italy")
  | "londra" | "london" -> Some ("london", "england", "uk")
  | _ -> None
;;

let location_of_args args =
  let params = args |> List.map ~f:Caml.String.trim |> List.map ~f:String.lowercase in
  match params with
  | city :: state :: country :: _ -> Some (city, state, country)
  | [known_city] -> resolve_known_city known_city
  | _ -> None
;;

(* Bot module *)
let register () = ()
let help () = "*Air quality*\n`aq [city] [state] [country]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("aq", args, chat_id, _) ->
    (match location_of_args args with
    | Some (city, state, country) ->
      get_air_quality ~chat_id ~city ~state ~country;
      true
    | _ -> false)
  | _ -> false
;;
