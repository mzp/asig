module Command : sig
  type t =
    | PrivMsg of string * string * string
    | Nick of string
end

module Reply : sig
  type t =
      Welcome of string * string
    | YourHost
    | Created
    | MyInfo of string * string
    | PrivMsg of string * string * string
end

val establish_server :
  string ->
  string ->
  on_recv:(((string * Reply.t) option -> unit) -> Command.t -> unit Lwt.t) ->
  action:(((string * Reply.t) option -> unit) -> unit Lwt.t) -> Lwt_io.server Lwt.t
