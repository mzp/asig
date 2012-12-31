module Command : sig
  type t =
    | PrivMsg of string * string * string
    | Nick    of string
    | Join    of string
end

module Reply : sig
  type t =
      Welcome of string * string
    | YourHost
    | Created
    | MyInfo  of string * string
    | Topic   of string * string
    | PrivMsg of string * string * string
end

val establish_server :
  string ->
  string ->
  action:(((string * Reply.t) option -> unit) -> Command.t Lwt_stream.t -> unit Lwt.t) -> Lwt_io.server Lwt.t
