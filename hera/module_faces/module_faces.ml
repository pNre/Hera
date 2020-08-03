open Async
open Images
open OImages
open Core

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
  = "caml_try_detect"

type error =
  | Http of Http.error
  | Image_process
  | Image_download

let detect_faces photo =
  detect_faces'
    photo
    (Sys.getenv_exn "FACES_CONFIG_PATH")
    (Sys.getenv_exn "FACES_WEIGHTS_PATH")
    (Sys.getenv_exn "FACES_EYES_MODEL_PATH")
;;

let is_waiting_for_image = ref false

let write_response_to_temp_file (_, body) =
  let output = Filename.temp_file "faces" "" in
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

let inset_rect f x y width_limit height_limit =
  let x0 = Int.clamp_exn (f.x - x) ~min:0 ~max:width_limit in
  let y0 = Int.clamp_exn (f.y - y) ~min:0 ~max:height_limit in
  let w0 = Int.clamp_exn (f.w + (x * 2)) ~min:0 ~max:width_limit in
  let h0 = Int.clamp_exn (f.h + (y * 2)) ~min:0 ~max:height_limit in
  { x = x0; y = y0; w = w0; h = h0 }
;;

let merge_eyes e1 e2 width_limit height_limit =
  let min_ex = Int.min e1.x e2.x in
  let min_ey = Int.min e1.y e2.y in
  let eho1 = e1.x + e1.w in
  let eho2 = e2.x + e2.w in
  let ev1 = e1.y + e1.h in
  let ev2 = e1.y + e2.h in
  let ew = Int.max eho1 eho2 - min_ex in
  let eh = Int.max ev1 ev2 - min_ey in
  inset_rect
    { x = min_ex; y = min_ey; w = ew; h = eh }
    ew
    (eh * 2)
    width_limit
    height_limit
;;

let inset_face f x y width_limit height_limit =
  let x0 = Int.clamp_exn (f.x - x) ~min:0 ~max:width_limit in
  let y0 = Int.clamp_exn (f.y - y) ~min:0 ~max:height_limit in
  let w0 = Int.clamp_exn (f.w + (x * 2)) ~min:0 ~max:width_limit in
  let h0 = Int.clamp_exn (f.h + (y * 2)) ~min:0 ~max:height_limit in
  { x = x0; y = y0; w = w0; h = h0 }
;;

let download_and_process_photo file_id =
  let download_photo photo_path =
    Telegram.download_file photo_path
    >>=? write_response_to_temp_file
    >>| Result.map_error ~f:(fun x -> Http x)
  in
  let process_photo photo =
    try
      let rects = detect_faces photo in
      let face = List.find rects ~f:(fun (_, eyes) -> List.length eyes = 2) in
      match face with
      | Some (face, [ e1; e2 ]) ->
        Logging.Module.info
          "Face: %s, L-eye: %s, R-eye: %s"
          (show_rect face)
          (show_rect e1)
          (show_rect e2);
        let image = load photo [] in
        let eyes = merge_eyes e1 e2 image#width image#height in
        let face_large = face in
        let face_small =
          inset_face
            face
            (-image#width / 10)
            (-image#height / 10)
            image#width
            image#height
        in
        let cropped_face_large =
          sub image face_large.x face_large.y face_large.w face_large.h
        in
        let cropped_face_small =
          sub image face_small.x face_small.y face_small.w face_small.h
        in
        let cropped_eyes = sub image eyes.x eyes.y eyes.w eyes.h in
        let photo_face_large = photo ^ "face_l" in
        let photo_face_small = photo ^ "face_s" in
        let photo_eyes = photo ^ "eyes" in
        let cropped_face_large = scale_image cropped_face_large 300 in
        let cropped_face_small = scale_image cropped_face_small 300 in
        let cropped_eyes = scale_image cropped_eyes 300 in
        cropped_face_large#save photo_face_large (Some Jpeg) [ Save_Quality 70 ];
        cropped_face_small#save photo_face_small (Some Jpeg) [ Save_Quality 70 ];
        cropped_eyes#save photo_eyes (Some Jpeg) [ Save_Quality 70 ];
        Ok [ photo_face_large; photo_face_small; photo_eyes ]
      | Some (_, eyes) ->
        Logging.Module.info "Face found, %i eyes" (List.length eyes);
        Error Image_process
      | _ -> Error Image_process
    with
    | ex ->
      Logging.Module.error "couldn't process image -> %s" (Exn.to_string ex);
      Error Image_process
  in
  Telegram.get_file file_id
  >>| Result.map_error ~f:(fun x -> Http x)
  >>=? (function
         | { file_path = Some path; _ } -> path |> download_photo
         | _ -> Deferred.return (Error Image_download))
  >>|? process_photo
  >>| Result.join
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
             Telegram.send_photo ~chat_id ~photo ~filename ~mimetype:"image/jpg"
             >>| ignore)
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
let help () = "*Faces*\n`fc`"

let on_update update =
  match Telegram.parse_update update with
  | `Command ("fc", _, chat_id, _) ->
    is_waiting_for_image := true;
    don't_wait_for (Telegram.send_message ~chat_id ~text:"Send me a picture" () >>| ignore);
    true
  | `Photos (photos, chat_id, _) when !is_waiting_for_image ->
    compress_photos chat_id photos
  | _ ->
    is_waiting_for_image := false;
    false
;;
