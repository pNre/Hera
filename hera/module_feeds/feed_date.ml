(* Taken from http://zoggy.github.io/ocamlrss/ *)

open Core

let int_of_month = function
  | "Jan" -> 1
  | "Feb" -> 2
  | "Mar" -> 3
  | "Apr" -> 4
  | "May" -> 5
  | "Jun" -> 6
  | "Jul" -> 7
  | "Aug" -> 8
  | "Sep" -> 9
  | "Oct" -> 10
  | "Nov" -> 11
  | "Dec" -> 12
  | _ -> invalid_arg "RFC822.int_of_month"
;;

let parse_offset = function
  | "UT" | "GMT" | "Z" -> 0
  | "EST" -> -5 * 60
  | "EDT" -> -4 * 60
  | "CST" -> -6 * 60
  | "CDT" -> -5 * 60
  | "MST" -> -7 * 60
  | "MDT" -> -6 * 60
  | "PST" -> -8 * 60
  | "PDT" -> -7 * 60
  | "A" -> -1 * 60
  | "M" -> -12 * 60
  | "N" -> 1 * 60
  | "Y" -> 12 * 60
  | str ->
    let intv = int_of_string str in
    let hours = intv / 100 |> abs in
    let mins = intv mod 100 |> abs in
    (if intv >= 0 then 1 else -1) * ((hours * 60) + mins)
;;

let parse_time ?(offset = 0) string =
  match String.split string ~on:':' with
  | [hr; mn] -> (int_of_string hr, int_of_string mn, 0), offset
  | [hr; mn; s] -> (int_of_string hr, int_of_string mn, int_of_string s), offset
  | _ -> invalid_arg "RFC822.parse_time"
;;

let of_rfc822 string =
  let weekday_and_date = String.lsplit2 string ~on:',' in
  let date = match weekday_and_date with Some (_, x) -> x | _ -> string in
  let components = date |> Caml.String.trim |> String.split ~on:' ' in
  let parse () =
    match components with
    | [day; month; year; time] ->
      Ptime.of_date_time
        ((int_of_string year, int_of_month month, int_of_string day), parse_time time)
    | [day; month; year; time; offset] ->
      Ptime.of_date_time
        ( (int_of_string year, int_of_month month, int_of_string day)
        , parse_time ~offset:(parse_offset offset * 60) time )
    | _ ->
      let time =
        string
        |> Time.of_string
        |> Time.to_span_since_epoch
        |> Time.Span.to_sec
        |> Ptime.of_float_s
      in
      if Option.is_none time then Logging.Module.error "Invalid date %s" string;
      time
  in
  parse
  |> Result.try_with
  |> Result.bind ~f:(Result.of_option ~error:(Invalid_argument "RFC822.date"))
;;
