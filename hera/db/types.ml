module Subscription = struct
  type t =
    { id : int [@default 0]
    ; subscriber_id : string
    ; type_id : string [@default ""]
    ; feed_url : string }
  [@@deriving make]

  let key subscription = subscription.subscriber_id ^ "," ^ subscription.feed_url
end

module Preference = struct
  type t =
    { owner_id : string
    ; key : string
    ; value : string }
  [@@deriving make]
end
