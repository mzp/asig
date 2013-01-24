val action : ((string * Asig.Irc.Reply.t) option -> unit) -> Asig.Irc.Command.t Lwt_stream.t -> unit Lwt.t
