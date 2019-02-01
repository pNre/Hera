open Async
open Core
open Cohttp

module Dispatcher : Bot.Module.t = struct
  type sense =
    { definitions : string list [@default []]
    ; short_definitions : string list [@default []]
    ; domains : string list [@default []] }
  [@@deriving of_yojson {strict = false}]

  type pronunciation =
    { audio_file : string option [@key "audioFile"] [@default None]
    ; dialects : string list [@default []]
    ; phonetic_notation : string option [@key "phoneticNotation"] [@default None]
    ; phonetic_spelling : string option [@key "phoneticSpelling"] [@default None] }
  [@@deriving of_yojson {strict = false}]

  type entry =
    { etymologies : string list [@default []]
    ; pronunciations : pronunciation list [@default []]
    ; senses : sense list [@default []] }
  [@@deriving of_yojson {strict = false}]

  type lexical_entry =
    { entries : entry list [@default []]
    ; language : string
    ; lexical_category : string [@key "lexicalCategory"]
    ; pronunciations : pronunciation list [@default []]
    ; text : string }
  [@@deriving of_yojson {strict = false}]

  type headword_entry =
    { id : string
    ; language : string
    ; lexical_entries : lexical_entry list [@key "lexicalEntries"]
    ; entry_type : string option [@key "type"] [@default None]
    ; pronunciations : pronunciation list [@default []]
    ; word : string }
  [@@deriving of_yojson {strict = false}]

  type retrieve_entry = {results : headword_entry list}
  [@@deriving of_yojson {strict = false}]

  let http_headers =
    ["app_id", Sys.getenv_exn "OXDICT_APP_ID"; "app_key", Sys.getenv_exn "OXDICT_APP_KEY"]
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
           let domains =
             if List.is_empty x.domains
             then ""
             else sprintf "_%s_" (String.concat ~sep:", " x.domains)
           in
           [defs; domains]
           |> List.filter ~f:(fun x -> not (String.is_empty x))
           |> String.concat ~sep:" - " )
  ;;

  let pronunciations_of_response response =
    let prons =
      response
      |> List.map ~f:(fun x -> x.lexical_entries)
      |> List.concat
      |> List.map ~f:(fun (x : lexical_entry) -> x.pronunciations)
      |> List.concat
      |> List.map ~f:(fun x -> x.phonetic_spelling)
      |> List.filter_map ~f:(fun x -> x)
      |> List.dedup_and_sort ~compare:String.ascending
    in
    if List.is_empty prons
    then None
    else Some ("Pronunciation: " ^ String.concat ~sep:", " prons)
  ;;

  let handle_failure chat_id err =
    Logging.Module.error "%s" err;
    let text = sprintf "No results (`%s`)" err in
    don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
  ;;

  let handle_success chat_id body =
    let result = body |> Yojson.Safe.from_string |> retrieve_entry_of_yojson in
    match result with
    | Ok {results} ->
      results
      |> pronunciations_of_response
      |> Option.iter ~f:(fun text ->
             don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore) );
      results
      |> definitions_of_response
      |> List.map ~f:(fun text -> ">" ^ text)
      |> List.map ~f:(fun text -> Telegram.send_message ~chat_id ~text () >>| ignore)
      |> Deferred.all_unit
      |> don't_wait_for
    | Error err -> handle_failure chat_id err
  ;;

  let search_term ~chat_id ~term =
    let encoded_term = term |> String.lowercase |> Uri.pct_encode in
    let path = sprintf "/api/v1/entries/en/%s" encoded_term in
    let uri = Uri.make ~scheme:"https" ~host:"od-api.oxforddictionaries.com" ~path () in
    Http.request `GET uri ~http_headers ()
    >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
    >>> function
    | Ok body -> handle_success chat_id body
    | Error (Request _) -> handle_failure chat_id "request"
    | Error (Response (Response.({status; _}), _)) ->
      handle_failure chat_id (Code.string_of_status status)
    | Error Format -> handle_failure chat_id "Invalid request"
  ;;

  (* Bot module *)
  let register () = ()
  let help () = "*English dictionary*\n`d [word]`"

  let on_update update =
    match update with
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.is_prefix t ~prefix:"d " ->
      let term = String.chop_prefix_exn t ~prefix:"d " in
      search_term ~chat_id ~term;
      true
    | _ -> false
  ;;
end
