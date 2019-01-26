open Async
open Core
open Syndic
open Types

type error =
  | Http of Http.http_error
  | Parse of Atom.Error.t

let get_feed_content s =
  let uri = Uri.of_string s.feed_url in
  let host = uri |> Uri.host |> fun host -> Option.value_exn host in
  let default_port = if Uri.scheme uri = Some "https" then 443 else 80 in
  let port = uri |> Uri.port |> Option.value ~default:default_port in
  let host_and_port = Host_and_port.create ~host ~port in
  let path = uri |> Uri.path in
  let request = Http.{host_and_port; http_method = `GET; http_headers = []; path} in
  Http.request request () >>|? fun (_, body) -> Bigbuffer.contents body
;;

let push_new_content ~title ~link ~subscription ~sent_item =
  Db.Main.insert_sent_item sent_item
  >>> fun _ ->
  let text = sprintf "%s\n%s" title link in
  let chat_id = Int64.of_string subscription.subscriber_id in
  don't_wait_for (Telegram.send_message ~chat_id ~text () >>| ignore)
;;

let send_content_if_needed content ~subscription =
  match content with
  | Some {title = Some title; link = Some link} ->
    Log.Global.info "Checking whether to send %s to %s" link subscription.subscriber_id;
    let sent_item = Types.{subscription_id = subscription.id; last_item_url = link} in
    Db.Main.find_sent_item sent_item
    >>> (function
    | Ok false ->
      Log.Global.info "Sending %s to %s" link subscription.subscriber_id;
      push_new_content ~title ~link ~subscription ~sent_item
    | _ -> Log.Global.info "%s already sent to %s" link subscription.subscriber_id)
  | _ -> ()
;;

let content_of_rss1 (feed : Syndic.Rss1.item) =
  {title = Some feed.title; link = Some (Uri.to_string feed.link)}
;;

let content_of_atom (feed : Syndic.Atom.entry) =
  let title =
    match feed.title with
    | Text title | Html (_, title) -> title
    | Xhtml _ -> "Unparsable title"
  in
  let link =
    feed.links |> List.hd |> Option.map ~f:(fun l -> Uri.to_string l.Atom.href)
  in
  {title = Some title; link}
;;

let content_of_rss2 (feed : Syndic.Rss2.item) =
  let title =
    match feed.story with
    | All (title, _, _) | Title title -> title
    | Description _ -> "Unparsable title"
  in
  {title = Some title; link = feed.link |> Option.map ~f:Uri.to_string}
;;

let latest_content_of_feed = function
  | `Rss1 rss1 -> rss1.Syndic.Rss1.item |> List.hd |> Option.map ~f:content_of_rss1
  | `Rss2 rss2 -> rss2.Syndic.Rss2.items |> List.hd |> Option.map ~f:content_of_rss2
  | `Atom atom -> atom.Syndic.Atom.entries |> List.hd |> Option.map ~f:content_of_atom
;;

let attempt_map_feed ~xmlbase content =
  let make_input () = Xmlm.make_input (`String (0, content)) in
  let parse_funs =
    [ (fun input -> `Rss1 (Rss1.parse ~xmlbase input))
    ; (fun input -> `Rss2 (Rss2.parse ~xmlbase input))
    ; (fun input -> `Atom (Atom.parse ~xmlbase input)) ]
  in
  let rec parse = function
    | f :: fs -> (try Ok (f (make_input ())) with _ -> parse fs)
    | [] -> Error (Parse ((0, 0), "Couldn't parse feed"))
  in
  parse parse_funs
;;

let begin_checking_subscription s =
  let check_for_new_entries () =
    get_feed_content s
    >>| Result.map_error ~f:(fun err -> Http err)
    >>| Result.bind ~f:(attempt_map_feed ~xmlbase:(Uri.of_string s.feed_url))
    >>|? latest_content_of_feed
    >>| Result.map ~f:(send_content_if_needed ~subscription:s)
    >>| function
    | Ok _ -> ()
    | Error (Http _err) -> Log.Global.error "Download of feed %s failed" s.feed_url
    | Error (Parse (_, error_string)) ->
      Log.Global.error "Parse of feed %s failed -> %s" s.feed_url error_string
  in
  Log.Global.info "Starting subscription checking task for %s" s.feed_url;
  let timespan = Time.Span.create ~min:10 () in
  Clock.every' timespan check_for_new_entries
;;

let add_subscription subscriber_id feed_url =
  let feed_uri = Uri.of_string feed_url in
  match Uri.host feed_uri with
  | Some _ ->
    let subscription =
      Types.
        { id = 0
        ; subscriber_id = Int64.to_string subscriber_id
        ; type_id = "Telegram"
        ; feed_url }
    in
    Log.Global.info "Adding subscription %s" feed_url;
    Db.Main.insert_subscription subscription
    >>> fun _ -> begin_checking_subscription subscription
  | _ -> Log.Global.error "Invalid uri %s" feed_url
;;
