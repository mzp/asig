type 'a result = ('a, string) Either.t Lwt.t

module Make : functor (Http : Http.S) -> sig
  module Api : sig
    type t = {
      api_key : string option;
      entry_point : string
    }

    val init  : ?api_key:string -> string -> t
  end

  module Room : sig
    type t = {
      id       : string;
      name     : string;
      nickname : string option
    }

    val make : string -> t
    val list : Api.t -> t list result
  end

  module Message : sig
    type t = {
      id          : string;
      body        : string;
      name        : string;
      screen_name : string;
      room        : Room.t
    }

    val list : Room.t -> Api.t -> t list result
    val post  : Room.t -> string -> Api.t -> unit result
  end

  module Event : sig
    type t = {
      name : string;
      args : string list
    }

    val on_message : f:(Message.t -> unit) -> Uri.t -> Room.t -> unit result
  end

  module MessagePusher : sig
    (* FIXME: support other pusher (e.g. socky, pusher)*)
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

  module Service : sig
    type t = {
      message_pusher : MessagePusher.t;
    }

    val info  : Api.t -> t result
  end
end
