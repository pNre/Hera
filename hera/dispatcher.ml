open Async
open Core
open Cohttp_async

let modules =
  [ (module Module_dictionary.Dispatcher : Bot.Module.t)
  ; (module Module_stocks.Dispatcher : Bot.Module.t)
  ; (module Module_feeds.Dispatcher : Bot.Module.t)
  ; (module Module_air_quality.Dispatcher : Bot.Module.t)
  ; (module Module_morejpeg.Dispatcher : Bot.Module.t) ]
;;

let register m =
  let module M = (val m : Bot.Module.t) in
  M.register ()
;;

let on_update m update =
  let module M = (val m : Bot.Module.t) in
  M.on_update update
;;

let register_modules () = List.iter modules ~f:register

let modules_help () =
  let module_help m =
    let module M = (val m : Bot.Module.t) in
    M.help ()
  in
  modules |> List.map ~f:module_help |> String.concat ~sep:"\n"
;;

let dispatch update =
  let handled =
    List.fold_left modules ~init:false ~f:(fun handled m -> on_update m update || handled)
  in
  (match handled, update with
  | false, {Telegram.message = Some {chat = {id = chat_id; _}; _}; _} ->
    let text = modules_help () in
    don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
  | _ -> ());
  Server.respond `OK
;;
