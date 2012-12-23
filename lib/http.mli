type 'a result = ('a, string) ElMonad.t

type socket_io_event =
  | Connection
  | Json of Tiny_json.Json.t

module type S = sig
  val get       : Uri.t -> string result
  val post      : Uri.t -> (string * string) list -> string result
  val socket_io : f:((socket_io_event -> unit) -> socket_io_event -> unit) -> Uri.t -> unit result
end

module Default : S
