open Core

type t = Show_links_preview
type value_type = Bool of bool

let key = function Show_links_preview -> "links/show_preview"
let of_key = function "links/show_preview" -> Some Show_links_preview | _ -> None

let of_key_exn key =
  match of_key key with Some pref -> pref | None -> failwith "Unknown key"
;;

let type_of_preference = function Show_links_preview -> `Bool
let description_of_preference = function Show_links_preview -> "Show links preview"
let default_value = function Show_links_preview -> Bool true
let string_of_value = function Bool b -> string_of_bool b

let value_of_string preference string =
  match preference with Show_links_preview ->
    string
    |> bool_of_string_opt
    |> Option.value_map
         ~f:(fun x -> Bool x)
         ~default:(default_value preference)
;;

let all = [Show_links_preview]
