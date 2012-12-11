module type S = sig
  val get : string -> string
end

type room = {
  room_id : string;
  room_name : string
}

module Make : functor (Http : S) -> sig
  type t

  val init  : string -> t
  val rooms : t -> room list
end
