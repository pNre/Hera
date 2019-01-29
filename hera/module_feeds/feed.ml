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
  | Other
[@@deriving show]

let resolve_uri xmlbase uri =
  let uri = Uri.of_string uri in
  match Uri.host uri with
  | Some _ -> Uri.to_string uri
  | None ->
    let scheme = Option.value (Uri.scheme xmlbase) ~default:"https" in
    uri |> Uri.resolve scheme xmlbase |> Uri.to_string
;;

let content_of_atom_entry xmlbase entry =
  let title =
    List.find_map entry ~f:(function Title title -> Some title | _ -> None)
  in
  let link =
    entry
    |> List.find_map ~f:(function
           | Link (attributes, _) -> Some (Caml.List.assoc "href" attributes)
           | _ -> None )
    |> Option.map ~f:(resolve_uri xmlbase)
  in
  match title, link with Some title, Some link -> Some {title; link} | _ -> None
;;

let content_of_rss_item xmlbase item =
  let title = List.find_map item ~f:(function Title title -> Some title | _ -> None) in
  let link =
    List.find_map item ~f:(function Link (_, uri) -> uri | _ -> None)
    |> Option.map ~f:(resolve_uri xmlbase)
  in
  match title, link with Some title, Some link -> Some {title; link} | _ -> None
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
    |> List.hd
  | Atom children ->
    List.filter_map children ~f:(function
        | Entry entry -> content_of_atom_entry xmlbase entry
        | _ -> None )
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
  let input = Xmlm.make_input (`String (0, content)) in
  try Ok (parse_feed xmlbase input) with exn ->
    Error (`Parse ((0, 0), sprintf "Couldn't parse feed, %s" (Exn.to_string exn)))
;;
