open Base
open Tiny_json
open Json_conv

type 'a result = ('a, string) ElMonad.t

module Make(H : Http.S) = struct
  module Api = struct
    type t = {
      api_key : string option;
      entry_point : string
    }

    let init ?api_key entry_point = {
      api_key;
      entry_point
    }

    let to_params params =
      params
      +> List.map (fun (key,name) -> Printf.sprintf "%s=%s" key name)
      +> String.concat "&"

    let api { entry_point; api_key  } path params =
      let params =
      (* append api_key field *)
        api_key
        +> BatOption.map (fun s -> ("api_key", s))
        +> BatOption.enum
        +> BatList.of_enum
        +> (@) params
        (* encode to GET parameters *)
        +> to_params
        (* prepend "?" *)
        +> function "" -> "" | s -> "?" ^ s
      in
      Printf.sprintf "%s/api/v1/%s.json%s" entry_point path params
      +> Uri.of_string
  end

  let http_get uri =
    H.get uri
    +> ElMonad.fmap Json.parse

  let conv f =
    ElMonad.lift (fun x ->
      match f x with
        | `Ok x -> `Ok x
        | `Error _ -> `Error "conversion error")

  module Room = struct
    type t (: Ignore_unknown_fields :) = {
      id       : string;
      name     : string;
      nickname : string option
    } with conv(json)

    type ts = t list with conv(json)

    let list t =
      Api.api t "room/list" []
      +> http_get
      +> conv ts_of_json

    let make id = {
      id; name = ""; nickname = None
    }

  end

  module Message = struct
    type t (: Ignore_unknown_fields :) = {
      id          : string;
      body        : string;
      name        : string;
      screen_name : string;
      room        : Room.t
    } with conv(json)

    type ts = t list with conv(json)

    let list { Room.id; _ } t =
      Api.api t "message/list" ["room_id", id]
      +> http_get
      +> conv ts_of_json

    let post { Room.id; _ } message t =
      let url =
        Api.api { t with Api.api_key = None } "message" []
      in
      H.post url [
        "room_id", id;
        "message", message;
        "api_key", BatOption.get t.Api.api_key
      ]
      +> ElMonad.fmap (const ())
  end

  module Event = struct
    type t = {
      name : string;
      args : string list
    } with conv(json)

    type content = {
      content : Message.t
    } with conv(json)

    let on_message ~f uri { Room.id; _ } =
      H.socket_io uri ~f:begin fun send -> function
        | Http.Connection ->
          send @@ Http.Json (json_of_t { name = "subscribe"; args = [ "as-" ^ id ] })
        | Http.Json json ->
          try
            match t_of_json_exn json  with
              | { name = "message_create"; args = [_; json]; _ } ->
                let { content; _ } =
                  content_of_json_exn @@ Json.parse json in
                f content
              | _ ->
                ()
          with _ ->
            ()
      end
  end

  module MessagePusher = struct
    (* FIXME: support other pusher (e.g. socky, pusher)*)
    module Param = struct
      type t = {
        url : string;
        key : string
      } with conv(json)
    end

    type t = {
      name  : string;
      param : Param.t
    } with conv(json)
  end

  module Service = struct
    type t = {
      message_pusher : MessagePusher.t;
    } with conv(json)

    let info t =
      Api.api t "service/info" []
      +> http_get
      +> conv t_of_json
  end
end
