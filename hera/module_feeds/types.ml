type feed_content =
  { title : string
  ; link : string
  ; date : Ptime.t }
[@@deriving show]

let sort_feed_content a b = Ptime.compare b.date a.date 
