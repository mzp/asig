open Base
open Tiny_json
open Json_conv

type t = {
  api_key : string option;
  entry_point : string
}

type 'a result = ('a, string) ElMonad.t

(* json for room *)
type room (: Ignore_unknown_fields :) = {
  room_id as "id": string;
  room_name as "name": string;
  nickname : string option
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

type content = {
  content : message
} with conv(json)

type event = {
  event_name as "name" : string;
  args : string list
} with conv(json)

module Make(H : Http.S) = struct
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

  let http_get uri =
    H.get uri
    +> ElMonad.fmap Json.parse

  let conv f =
    ElMonad.lift (fun x ->
      match f x with
        | `Ok x -> `Ok x
        | `Error _ -> `Error "conversion error")

  let rooms t =
    api t "room/list" []
    +> http_get
    +> conv rooms_of_json

  let messages room_id t =
    api t "message/list" ["room_id", room_id]
    +> http_get
    +> conv messages_of_json

  let post room_id message t =
    let url =
      api { t with api_key = None } "message" []
    in
    H.post url [
      "room_id", room_id;
      "message", message;
      "api_key", BatOption.get t.api_key
    ]
    +> ElMonad.fmap (const ())

  let on_message ~f uri { room_id; _ } =
    H.socket_io uri ~f:begin fun send -> function
      | Http.Connection ->
        send @@ Http.Json (json_of_event { event_name = "subscribe"; args = [
          "as-" ^ room_id
        ]})
      | Http.Json json ->
        try
          match event_of_json_exn json  with
            | { event_name = "message_create"; args = [_; json]; _ } ->
              let { content; _ } =
                content_of_json_exn @@ Json.parse json in
              f content
            | _ ->
              ()
        with _ ->
          ()
    end
end
