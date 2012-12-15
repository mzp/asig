open Base
open Tiny_json
open Json_conv

type t = {
  api_key : string option;
  entry_point : string
}

type 'a result =
    ('a, Tiny_json.Json.t) Meta_conv.Types.Result.t

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

module type S = sig
  val get : string -> string
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
    Json.parse (Http.get url)

  let rooms t =
    api t "room/list" []
    +> http_get
    +> rooms_of_json

  let messages room_id t =
    api t "message/list" ["room_id", room_id]
    +> http_get
    +> messages_of_json
end
