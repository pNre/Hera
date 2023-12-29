open Async
open Core
open Images

let is_waiting_for_image = ref false
let quality = ref 70

let write_response_to_temp_file (_, body) =
  let output = Filename_unix.temp_file "morejpeg" "" in
  Logging.Module.info "Writing image in %s" output;
  let pipe_r = Http.pipe_of_body body in
  Writer.with_file output ~f:(fun writer ->
    Writer.transfer writer pipe_r (Writer.write writer))
  >>| fun _ -> Result.return output
;;

let download_and_process_photo file_id =
  let open Deferred.Result.Let_syntax in
  let download_photo photo_path =
    Telegram.download_file photo_path >>=? write_response_to_temp_file
  in
  let process_photo photo =
    try
      let image = load photo [] in
      save photo (Some Jpeg) [ Save_Quality !quality ] image;
      Result.return photo
    with
    | ex ->
      Logging.Module.error "couldn't process image -> %s" (Exn.to_string ex);
      Result.fail (`Err "Failed to process image")
  in
  match%bind Telegram.get_file file_id with
  | { file_path = Some path; _ } ->
    let%bind photo = path |> download_photo in
    Deferred.return (process_photo photo)
  | _ -> Deferred.Result.fail (`Err "Failed to download image")
;;

let compress_photos chat_id photos =
  let process file_id =
    download_and_process_photo file_id
    >>=? (fun f -> Reader.file_contents f >>| Result.return)
    >>= fun content ->
    match content with
    | Ok photo ->
      Telegram.send_photo ~chat_id ~photo ~filename:"more.jpg" ~mimetype:"image/jpg"
      >>| ignore
    | _ -> Deferred.unit
  in
  let compare_photos a b =
    (a.Telegram.width * a.height) - (b.Telegram.width * b.height)
  in
  let photo = photos |> List.sort ~compare:compare_photos |> List.last in
  match photo with
  | Some Telegram.{ file_id; width = _; _ } ->
    don't_wait_for (process file_id);
    true
  | None -> false
;;

(* Bot module *)
let register () = ()
let help () = "*More jpeg*\n`mj [quality]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("mj", args, chat_id, _) ->
    is_waiting_for_image := true;
    quality
      := args
         |> List.last
         |> Option.map ~f:Stdlib.String.trim
         |> Option.map ~f:int_of_string_opt
         |> Option.join
         |> Option.map ~f:(fun quality -> min 100 (max 1 quality))
         |> Option.value ~default:70;
    Telegram.send_message_don't_wait ~chat_id ~text:"Send me a picture" ();
    true
  | `Photos (photos, chat_id, _) when !is_waiting_for_image ->
    compress_photos chat_id photos
  | _ ->
    is_waiting_for_image := false;
    false
;;
