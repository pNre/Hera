open Async
open Core
open Syndic
open Types

let send_content_if_needed content ~subscription ~send =
  match content with
  | Some {title = Some title; link = Some link} ->
    Log.Global.info "Checking whether to send %s to %s" link subscription.subscriber_id;
    let sent_item = Types.{subscription_id = subscription.id; last_item_url = link} in
    Db.find_sent_item sent_item
    >>| Result.map_error ~f:(fun e -> `Db e)
    >>|? not
    >>|? Result.ok_if_true ~error:`Sent
    >>| Result.join
    >>=? (fun _ ->
           Log.Global.info "Sending %s to %s" link subscription.subscriber_id;
           Db.insert_sent_item sent_item >>| Result.map_error ~f:(fun e -> `Db e) )
    >>> Result.iter ~f:(fun _ -> send (sprintf "%s\n%s" title link))
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
    | [] -> Error (`Parse ((0, 0), "Couldn't parse feed"))
  in
  parse parse_funs
;;

let begin_checking_subscription subscription send =
  let string_of_request_error = function
    | `Malformed_response err -> err
    | `Invalid_response_body_length _ -> "Invalid response body length"
    | `Exn exn -> Exn.to_string exn
  in
  let check_for_new_entries () =
    Http.request `GET (Uri.of_string subscription.feed_url) [] ()
    >>|? (fun (_, body) -> Bigbuffer.contents body)
    >>| Result.map_error ~f:(fun err -> `Http err)
    >>| Result.bind ~f:(attempt_map_feed ~xmlbase:(Uri.of_string subscription.feed_url))
    >>|? latest_content_of_feed
    >>| Result.map ~f:(send_content_if_needed ~subscription ~send)
    >>| function
    | Ok _ -> ()
    | Error (`Http (Request err)) ->
      Log.Global.error
        "Download of feed %s failed -> %s"
        subscription.feed_url
        (string_of_request_error err)
    | Error (`Http _err) ->
      Log.Global.error "Download of feed %s failed" subscription.feed_url
    | Error (`Parse (_, error_string)) ->
      Log.Global.error "Parse of feed %s failed -> %s" subscription.feed_url error_string
  in
  Log.Global.info "Starting subscription checking task for %s" subscription.feed_url;
  let timespan = Time.Span.create ~min:10 () in
  Clock.every' timespan check_for_new_entries
;;

let add_subscription ~subscriber_id ~feed_url ~reply =
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
    let map_db_error e = `Db e in
    Db.find_subscription ~subscriber_id:subscription.subscriber_id ~feed_url
    >>| Result.map_error ~f:map_db_error
    >>|? not
    >>|? Result.ok_if_true ~error:`Result
    >>| Result.join
    >>=? (fun _ ->
           Log.Global.info "Adding subscription %s" feed_url;
           Db.insert_subscription subscription >>| Result.map_error ~f:map_db_error )
    >>> Result.iter ~f:(fun () -> begin_checking_subscription subscription reply)
  | _ -> Log.Global.error "Invalid uri %s" feed_url
;;

let remove_subscription ~subscriber_id ~feed_url ~reply =
  Log.Global.info
    "Removing subscription %s for %s"
    feed_url
    (Int64.to_string subscriber_id);
  Db.delete_subscription ~subscriber_id:(Int64.to_string subscriber_id) ~feed_url
  >>> fun _ -> reply "Feed removed"
;;

let list_subscriptions ~reply =
  let send_subscriptions subscriptions =
    let text =
      subscriptions
      |> List.map ~f:(fun subscription -> subscription.feed_url)
      |> String.concat ~sep:"\n"
    in
    reply text
  in
  Db.subscriptions () >>> Result.iter ~f:send_subscriptions
;;
