open Async
open Images
open OImages
open Core
module Image_id = Core.Unique_id.Int63 ()

type rect =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }
[@@deriving show]

external detect_faces'
  :  string
  -> string
  -> string
  -> string
  -> (rect * rect list) list
  = "caml_detect_faces"

let detect_faces photo =
  detect_faces'
    photo
    (Sys.getenv_exn "FACES_CONFIG_PATH")
    (Sys.getenv_exn "FACES_WEIGHTS_PATH")
    (Sys.getenv_exn "FACES_EYES_MODEL_PATH")
;;

let is_waiting_for_image = ref false
let default_quality = 70
let quality = ref default_quality
let scaled_width = 300

let write_response_to_temp_file (_, body) =
  let output = Filename_unix.temp_file "faces" "" in
  Logging.Module.info "Writing image in %s" output;
  let pipe_r = Http.pipe_of_body body in
  Writer.with_file output ~f:(fun writer ->
    Writer.transfer writer pipe_r (Writer.write writer))
  >>| fun _ -> Result.return output
;;

let scale_image image width =
  let height =
    Float.of_int image#height *. Float.of_int width /. Float.of_int image#width
    |> Float.round_nearest
    |> Float.to_int
  in
  (rgb24 image)#resize None width height
;;

let landmarks_rect e1 e2 mouth =
  let min3 a b c = Int.min (Int.min a b) c in
  let max3 a b c = Int.max (Int.max a b) c in
  let r_max_x rect = rect.x + rect.w in
  let r_max_y rect = rect.y + rect.h in
  let min_x = min3 e1.x e2.x mouth.x in
  let min_y = min3 e1.y e2.y mouth.y in
  let max_x = max3 (r_max_x e1) (r_max_x e2) (r_max_x mouth) in
  let max_y = max3 (r_max_y e1) (r_max_y e2) (r_max_y mouth) in
  { x = min_x; y = min_y; w = max_x - min_x; h = max_y - min_y }
;;

let inset_face f x y width_limit height_limit =
  let x0 = Int.clamp_exn (f.x - x) ~min:0 ~max:width_limit in
  let y0 = Int.clamp_exn (f.y - y) ~min:0 ~max:height_limit in
  let w0 = Int.clamp_exn (f.w + (x * 2)) ~min:0 ~max:width_limit in
  let h0 = Int.clamp_exn (f.h + (y * 2)) ~min:0 ~max:height_limit in
  { x = x0; y = y0; w = w0; h = h0 }
;;

let download_and_process_photo file_id =
  let open Deferred.Result.Let_syntax in
  let download_photo photo_path =
    Telegram.download_file photo_path >>=? write_response_to_temp_file
  in
  let process_photo photo =
    try
      let rects = detect_faces photo in
      let face = List.find rects ~f:(fun (_, landmarks) -> List.length landmarks = 3) in
      match face with
      | Some (face, [ e1; e2; mouth ]) ->
        Logging.Module.info
          "Face: %s, L-eyebrow: %s, R-eyebrow: %s, mouth: %s"
          (show_rect face)
          (show_rect e1)
          (show_rect e2)
          (show_rect mouth);
        let image = load photo [] in
        Logging.Module.info "W: %d, H: %d" image#width image#height;
        let face_zoomed = landmarks_rect e1 e2 mouth in
        let face_large = face in
        let face_small =
          inset_face face (face.w / 3) (face.h / 3) image#width image#height
        in
        let cropped rect = sub image rect.x rect.y rect.w rect.h in
        let cropped_face_large = cropped face_large in
        let cropped_face_small = cropped face_small in
        let cropped_face_zoomed = cropped face_zoomed in
        Result.return
          ((if !quality < default_quality
           then
             [ rgb24 image
             ; scale_image cropped_face_small scaled_width
             ; scale_image cropped_face_large scaled_width
             ; scale_image cropped_face_zoomed scaled_width
             ]
           else
             [ scale_image cropped_face_small scaled_width
             ; scale_image cropped_face_large scaled_width
             ; scale_image cropped_face_zoomed scaled_width
             ])
          |> List.map ~f:(fun image ->
               let name = photo ^ (Image_id.create () |> Image_id.to_string) in
               image#save name (Some Jpeg) [ Save_Quality !quality ];
               name))
      | Some (_, landmarks) ->
        Logging.Module.info "Face found, %i landmarks" (List.length landmarks);
        Result.fail (`Err "Failed to process image")
      | _ -> Result.fail (`Err "Failed to process image")
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
    >>=? (fun photos ->
           photos |> List.map ~f:Reader.file_contents |> Deferred.all >>| Result.return)
    >>= fun content ->
    match content with
    | Ok photos ->
      photos
      |> Deferred.List.iteri ~f:(fun index photo ->
           let filename = sprintf "fc_%i.jpg" index in
           Telegram.send_photo ~chat_id ~photo ~filename ~mimetype:"image/jpg" >>| ignore)
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
let help () = "*Faces*\n`fc [quality?]`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("fc", args, chat_id, _) ->
    is_waiting_for_image := true;
    quality
      := args
         |> List.last
         |> Option.map ~f:Caml.String.trim
         |> Option.map ~f:int_of_string_opt
         |> Option.join
         |> Option.map ~f:(fun quality -> min 100 (max 1 quality))
         |> Option.value ~default:default_quality;
    Telegram.send_message_don't_wait ~chat_id ~text:"Send me a picture" ();
    true
  | `Photos (photos, chat_id, _) when !is_waiting_for_image ->
    compress_photos chat_id photos
  | _ ->
    is_waiting_for_image := false;
    false
;;
