type subscription =
  { id : int
  ; subscriber_id : string
  ; type_id : string
  ; feed_url : string }

type sent_item =
  { subscription_id : int
  ; last_item_url : string }

type feed_content =
  { title : string option
  ; link : string option }
