open Base
open Tiny_json
open Json_conv

type t = {
  api_key : string option;
  entry_point : string
}

type 'a result = ('a, string) Either.t


(* json for room *)
type room (: Ignore_unknown_fields :) = {
  room_id as "id": string;
  room_name as "name": string
} with conv(json)

type rooms = room list with conv(json)

(* json for message *)
type message (: Ignore_unknown_fields :) = {
  message_id as "id" : string;
  body : string;
  name : string;
  screen_name : string;
  room : room
} with conv(json)

type messages = message list with conv(json)

(* socket io event *)
type content = {
  content : message
} with conv(json)

type event = {
  event_name as "name" : string;
  args : string list
} with conv(json)

type socket_io_event = [
  `Connection
| `Json of Tiny_json.Json.t ]

module type S = sig
  val get       : string -> (string, string) Either.t
  val post      : string -> (string * string) list -> (string, string) Either.t
  val socket_io : f:((string -> unit) -> socket_io_event -> unit) -> string -> unit result
end

module Make(Http : S) = struct
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

  let http_get url =
    Http.get url
    +> Either.fmap (fun s -> Json.parse s)

  let lift f x =
    match f x with
      | `Ok x -> `Ok x
      | `Error _ -> `Error "conversion error"

  let rooms t =
    let open Either in
    api t "room/list" []
    +> http_get
    >>= lift rooms_of_json

  let messages room_id t =
    let open Either in
    api t "message/list" ["room_id", room_id]
    +> http_get
    >>= lift messages_of_json

  let post room_id message t =
    let open Either in
    let url =
      api { t with api_key = None } "message" []
    in
    Http.post url @@ [
      "room_id", room_id;
      "message", message;
      "api_key", BatOption.get t.api_key
    ]
    >>= const (`Ok ())

  let on_message f url =
    Http.socket_io url ~f:begin fun send -> function
      | `Connection ->
        let json =
          JsonUtil.string_of_json @@ json_of_event { event_name = "subscribe"; args = [
            "as-50d07248489e0117e6000003"
          ]}
        in
        print_endline json;
        send ("5:::"^json)
(* { \"name\":\"subscribe\", \"args\":[\"as-50d07248489e0117e6000003\"]}"*)
      | `Json json ->
        let open Either in
        let _ =
          event_of_json json
          >>= function
            | { event_name = "message_create"; args=[_; json] } ->
              Json.parse json
              +> lift content_of_json
              +> Either.fmap (fun { content; _ } -> f content)
              +> Either.return
            | _ ->
              Either.return (`Error "unknown event")
        in
        ()
    end
end
