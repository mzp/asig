module Message : sig
  type t = {
    prefix  : string option;
    command : string;
    params  : string list;
  }
  val to_string   : t -> string
  val from_string : string -> t
end

module Command : sig
  type t =
    | PrivMsg of string * string * string
    | Nick    of string
    | Join    of string

  val from_message : Message.t -> t option
end

module Reply : sig
  type t =
      Welcome of string * string
    | YourHost
    | Created
    | MyInfo  of string * string
    | Topic   of string * string
    | PrivMsg of string * string * string

  val to_message : string -> t -> Message.t
end

val establish_server :
  string ->
  string ->
  action:(((string * Reply.t) option -> unit) -> Command.t Lwt_stream.t -> unit Lwt.t) -> Lwt_io.server Lwt.t
