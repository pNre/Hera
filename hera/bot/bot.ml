module type Module = sig
  val register: unit -> unit
  val help: unit -> string
  val on_update: Telegram.update -> bool
end
