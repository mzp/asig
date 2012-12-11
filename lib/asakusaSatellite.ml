open Base
open Tiny_json
open Json_conv

module type S = sig
  val get : string -> string
end

type room (: Ignore_unknown_fields :) = {
  room_id as "id": string;
  room_name as "name": string
} with conv(json)

type rooms = room list with conv(json)

module Make(Http : S) = struct
  type t = {
    entry_point : string
  }

  let init entry_point = {
    entry_point
  }

  let api { entry_point } path =
    Printf.sprintf "%s/api/v1/%s.json" entry_point path

  let rooms t =
    api t "room/list"
    +> Http.get
    +> Json.parse
    +> rooms_of_json_exn
end
