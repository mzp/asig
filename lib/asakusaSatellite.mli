type t
type 'a result = ('a, string) Either.t Lwt.t

type room = {
  room_id : string;
  room_name : string;
  nickname : string option
}

type message = {
  message_id : string;
  body : string;
  name : string;
  screen_name : string;
  room : room
}

module MessagePusher : sig
  module Param : sig
    type t = {
      url : string;
      key : string
    }
  end

  type t = {
    name  : string;
    param : Param.t
  }
end

module Info : sig
  type t = {
    message_pusher : MessagePusher.t;
  }
end

module Make : functor (Http : Http.S) -> sig
  val init  : ?api_key:string -> string -> t
  val rooms : t -> room list result
  val messages : string -> t -> message list result
  val info  : t -> Info.t result
  val post  : string -> string -> t -> unit result
  val on_message : f:(message -> unit) -> Uri.t -> room -> unit result
end
