type subscription =
  { id : int
  ; subscriber_id : string
  ; type_id : string
  ; feed_url : string }
  [@@deriving show]

type sent_item =
  { subscription_id : int
  ; last_item_url : string }
  [@@deriving show]

type feed_content =
  { title : string 
  ; link : string }
  [@@deriving show]
