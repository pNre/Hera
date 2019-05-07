open Core
open Types

type feed =
  | Atom of feed list
  | RSS of feed list
  | Channel of feed list
  | Entry of feed list
  | Item of feed list
  | Title of string
  | Data of string
  | Link of (string * string) list * string option
  | Date of string
  | Guid of string
  | Other
[@@deriving show]

let resolve_uri ~xmlbase uri =
  let uri = Uri.of_string uri in
  match Uri.host uri with
  | Some _ -> Uri.to_string uri
  | None ->
    let scheme = Option.value (Uri.scheme xmlbase) ~default:"https" in
    uri |> Uri.resolve scheme xmlbase |> Uri.to_string
;;

let map_title = function Title title -> Some title | _ -> None
let map_guid = function Guid guid -> Some guid | _ -> None
let map_date mapping date = match date with Date date -> mapping date | _ -> None

let map_atom_link ~xmlbase entry =
  match entry with
  | Link (attributes, _) ->
    Some (Caml.List.assoc "href" attributes |> resolve_uri ~xmlbase)
  | _ -> None
;;

let map_rss_link ~xmlbase item =
  match item with Link (_, uri) -> Option.map uri ~f:(resolve_uri ~xmlbase) | _ -> None
;;

let make_content ~title ~link ~date ~guid =
  match title, link, date with
  | Some title, Some link, Some date -> Some {title; link; date; guid}
  | _ -> None
;;

let content_of_atom_entry xmlbase entry =
  let title = List.find_map entry ~f:map_title in
  let date_mapping x =
    x |> Ptime.of_rfc3339 |> Result.ok |> Option.map ~f:(fun (date, _, _) -> date)
  in
  let date = List.find_map entry ~f:(map_date date_mapping) in
  let link = List.find_map entry ~f:(map_atom_link ~xmlbase) in
  let guid = List.find_map entry ~f:map_guid in
  make_content ~title ~link ~date ~guid
;;

let content_of_rss_item xmlbase item =
  let title = List.find_map item ~f:map_title in
  let date_mapping = Fn.compose Result.ok Feed_date.of_rfc822 in
  let date = List.find_map item ~f:(map_date date_mapping) in
  let link = List.find_map item ~f:(map_rss_link ~xmlbase) in
  let guid = List.find_map item ~f:map_guid in
  make_content ~title ~link ~date ~guid
;;

let latest_content_of_feed xmlbase feed =
  match feed with
  | RSS children ->
    children
    |> List.find_map ~f:(function Channel channel -> Some channel | _ -> None)
    |> Option.value ~default:[]
    |> List.filter_map ~f:(function
           | Item item -> content_of_rss_item xmlbase item
           | _ -> None )
    |> List.sort ~compare:sort_feed_content
    |> List.hd
  | Atom children ->
    List.filter_map children ~f:(function
        | Entry entry -> content_of_atom_entry xmlbase entry
        | _ -> None )
    |> List.sort ~compare:sort_feed_content
    |> List.hd
  | _ -> None
;;

let parse_feed xmlbase input =
  let data_children children =
    children |> List.find_map ~f:(function Data d -> Some d | _ -> None)
  in
  let doc =
    Xmlm.input_doc_tree
      ~el:(fun tag children ->
        match tag with
        | (_, "rss"), _ -> RSS children
        | (_, "feed"), _ -> Atom children
        | (_, "channel"), _ -> Channel children
        | (_, "entry"), _ -> Entry children
        | (_, "item"), _ -> Item children
        | (_, "guid"), _ ->
          (match data_children children with Some guid -> Guid guid | _ -> Other)
        | (_, "updated"), _ | (_, "pubDate"), _ ->
          (match data_children children with Some date -> Date date | _ -> Other)
        | (_, "title"), _ ->
          (match data_children children with Some title -> Title title | _ -> Other)
        | (_, "link"), attributes ->
          let attributes =
            List.map attributes ~f:(fun ((_, name), data) -> name, data)
          in
          let data = data_children children in
          Link (attributes, data)
        | _ -> Other )
      ~data:(fun data -> Data data)
      input
  in
  let _, data = doc in
  latest_content_of_feed xmlbase data
;;

let parse ~xmlbase content =
  let content = Caml.String.trim content in
  let input = Xmlm.make_input (`String (0, content)) in
  try Ok (parse_feed xmlbase input) with Xmlm.Error (_, error) ->
    Error (`Parse ((0, 0), sprintf "Couldn't parse feed, %s" (Xmlm.error_message error)))
;;
