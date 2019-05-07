type feed_content =
  { title : string
  ; guid : string option
  ; link : string
  ; date : Ptime.t
  }
[@@deriving show]

let sort_feed_content a b = Ptime.compare b.date a.date

let identifier_of_feed_content = function
  | { guid = Some guid; _ } -> guid
  | { link; _ } -> link
;;
