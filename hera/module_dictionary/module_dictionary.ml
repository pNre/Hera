open Async
open Core

type domain = { text : string } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type sense =
  { definitions : string list [@default []]
  ; short_definitions : string list [@default []]
  ; domains : domain list [@default []]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type pronunciation =
  { audio_file : string option [@key "audioFile"] [@default None]
  ; dialects : string list [@default []]
  ; phonetic_notation : string option [@key "phoneticNotation"] [@default None]
  ; phonetic_spelling : string option [@key "phoneticSpelling"] [@default None]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type entry =
  { etymologies : string list [@default []]
  ; pronunciations : pronunciation list [@default []]
  ; senses : sense list [@default []]
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type lexical_entry =
  { entries : entry list [@default []]
  ; language : string
  ; pronunciations : pronunciation list [@default []]
  ; text : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type headword_entry =
  { id : string
  ; language : string
  ; lexical_entries : lexical_entry list [@key "lexicalEntries"]
  ; entry_type : string option [@key "type"] [@default None]
  ; pronunciations : pronunciation list [@default []]
  ; word : string
  }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

type retrieve_entry = { results : headword_entry list }
[@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

let headers =
  [ "app_id", Sys.getenv_exn "OXDICT_APP_ID"; "app_key", Sys.getenv_exn "OXDICT_APP_KEY" ]
;;

let definitions_of_response response =
  response
  |> List.map ~f:(fun x -> x.lexical_entries)
  |> List.concat
  |> List.map ~f:(fun x -> x.entries)
  |> List.concat
  |> List.map ~f:(fun x -> x.senses)
  |> List.concat
  |> List.map ~f:(fun x ->
       let defs = String.concat ~sep:"\n" x.definitions in
       let domains = List.map x.domains ~f:(fun x -> x.text) in
       let domains =
         if List.is_empty domains
         then ""
         else sprintf "_%s_" (String.concat ~sep:", " domains)
       in
       [ defs; domains ]
       |> List.filter ~f:(Fn.non String.is_empty)
       |> String.concat ~sep:" - ")
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let pronunciations_of_response response =
  let prons =
    response
    |> List.map ~f:(fun x -> x.lexical_entries)
    |> List.concat
    |> List.map ~f:(fun (x : lexical_entry) -> x.pronunciations)
    |> List.concat
    |> List.map ~f:(fun x -> x.phonetic_spelling)
    |> List.filter_map ~f:Fn.id
    |> List.dedup_and_sort ~compare:String.ascending
  in
  if List.is_empty prons
  then None
  else Some ("Pronunciation: " ^ String.concat ~sep:", " prons)
;;

let handle_failure chat_id err =
  let error_message =
    match err with
    | `Exn exn -> Exn.to_string exn
    | `Http_response_error (code, _) -> sprintf "Http error %d" code
  in
  Logging.Module.error "%s" error_message;
  let text = sprintf "No results (`%s`)" error_message in
  Telegram.send_message_don't_wait ~chat_id ~text ()
;;

let handle_success chat_id body =
  let send text = Telegram.send_message ~chat_id ~text () >>| ignore in
  let result =
    Result.try_with (fun () -> body |> Jsonaf.of_string |> retrieve_entry_of_jsonaf)
  in
  match result with
  | Ok { results } ->
    let pronunciation = results |> pronunciations_of_response in
    let definitions = results |> definitions_of_response in
    Option.value_map pronunciation ~f:send ~default:Deferred.unit
    >>= (fun _ -> List.map definitions ~f:send |> Deferred.all_unit)
    |> don't_wait_for
  | Error err -> handle_failure chat_id (`Exn err)
;;

let search_term ~chat_id ~term =
  let encoded_term = term |> String.lowercase |> Uri.pct_encode in
  let path = sprintf "/api/v2/entries/en-gb/%s" encoded_term in
  let uri = Uri.make ~scheme:"https" ~host:"od-api.oxforddictionaries.com" ~path () in
  Http.request' `GET uri ~headers ()
  >>> function
  | Ok (_, body) -> handle_success chat_id body
  | Error err -> handle_failure chat_id err
;;

(* Bot module *)
let register () = ()
let help () = "*English dictionary*\n`d [word]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("d", term :: _, chat_id, _) ->
    search_term ~chat_id ~term;
    true
  | _ -> false
;;
