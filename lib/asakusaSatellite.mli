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

module type S = sig
  val get : string -> string result
  val post : string -> (string * string) list -> string result
end

module Make : functor (Http : S) -> sig
  val init  : ?api_key:string -> string -> t
  val rooms : t -> room list result
  val messages : string -> t -> message list result
  val post  : string -> string -> t -> unit result
end
