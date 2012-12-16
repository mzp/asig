type t
type 'a result = ('a, string) Either.t

type room = {
  room_id : string;
  room_name : string
}

type message = {
  message_id : string;
  body : string;
  name : string;
  screen_name : string;
  room : room
}

type socket_io_event = [
  `Connection
| `Json of Tiny_json.Json.t ]

module type S = sig
  val get       : string -> (string, string) Either.t
  val post      : string -> (string * string) list -> (string, string) Either.t
  val socket_io : f:((string -> unit) -> socket_io_event -> unit) -> string -> unit result
end

module Make : functor (Http : S) -> sig
  val init  : ?api_key:string -> string -> t
  val rooms : t -> room list result
  val messages : string -> t -> message list result
  val post  : string -> string -> t -> unit result
  val on_message : (message -> unit) -> string -> unit result
end
