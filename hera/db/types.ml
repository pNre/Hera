module Subscription = struct
  type t =
    { id : int [@default 0]
    ; subscriber_id : string
    ; type_id : string [@default ""]
    ; feed_url : string }
  [@@deriving show, make]

  let key subscription = subscription.subscriber_id ^ "," ^ subscription.feed_url
end

module Sent_item = struct
  type t =
    { subscription_id : int
    ; last_item_url : string }
  [@@deriving show, make]
end
