open Async
open Core
open Images

module Dispatcher : Bot.Module.t = struct
  let is_waiting_for_image = ref false
  let quality = ref 70

  let write_response_to_temp_file (_, body) =
    let output = Filename.temp_file "morejpeg" "" in
    Logging.Module.info "Writing image in %s" output;
    let pipe_r = Http.pipe_of_body body in
    Writer.with_file output ~f:(fun writer ->
        Writer.transfer writer pipe_r (Writer.write writer) )
    >>| fun _ -> Result.return output
  ;;

  let download_and_process_photo file_id =
    let download_photo photo_path =
      Telegram.download_file photo_path >>=? write_response_to_temp_file
    in
    let process_photo photo =
      try
        let image = load photo [] in
        save photo (Some Jpeg) [Save_Quality !quality] image;
        Some photo
      with ex ->
        Logging.Module.error "couldn't process image -> %s" (Exn.to_string ex);
        None
    in
    Telegram.get_file file_id
    >>=? (fun {file_path; _} ->
           Option.value_map
             file_path
             ~default:(Deferred.return (Result.fail Http.Format))
             ~f:download_photo )
    >>| function
    | Ok photo -> process_photo photo
    | Error _e ->
      Logging.Module.error "couldn't get file";
      None
  ;;

  let process_photos chat_id photos =
    let photo =
      photos
      |> List.sort
           ~compare:Telegram.(fun a b -> (a.width * a.height) - (b.width * b.height))
      |> List.last
    in
    match photo with
    | Some Telegram.({file_id; width = _; _}) ->
      don't_wait_for
        ( download_and_process_photo file_id
        >>= Option.value_map
              ~f:(fun f -> Reader.file_contents f >>| Option.some)
              ~default:(Deferred.return None)
        >>= fun content ->
        match content with
        | Some photo ->
          Telegram.send_photo ~chat_id ~photo ~filename:"more.jpg" ~mimetype:"image/jpg" >>| ignore
        | None -> Deferred.unit );
      true
    | None -> false
  ;;

  (* Bot module *)
  let register () = ()
  let help () = "*More jpeg*\n`mj [quality]`"

  let on_update update =
    match update with
    | {Telegram.message = Some {chat = {id = chat_id; _}; text = Some t; _}; _}
      when String.Caseless.is_prefix t ~prefix:"mj" ->
      is_waiting_for_image := true;
      quality :=
        t
        |> String.split ~on:' '
        |> List.last
        |> Option.map ~f:Caml.String.trim
        |> Option.map ~f:int_of_string_opt
        |> Option.join
        |> Option.map ~f:(fun quality -> min 100 (max 1 quality))
        |> Option.value ~default:70;
      don't_wait_for
        (Telegram.send_message ~chat_id ~text:"Send me a picture" () >>| ignore);
      true
    | {Telegram.message = Some {chat = {id = chat_id; _}; photo = photos; _}; _}
      when !is_waiting_for_image = true -> process_photos chat_id photos
    | _ ->
      is_waiting_for_image := false;
      false
  ;;
end
