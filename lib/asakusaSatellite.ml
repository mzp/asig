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
    api_key : string option;
    entry_point : string
  }

  let init ?api_key entry_point = {
    api_key;
    entry_point
  }

  let api { entry_point; api_key  } path =
    let params =
      match api_key with
        | Some s -> Printf.sprintf "?api_key=%s" s
        | None   -> "" in
    Printf.sprintf "%s/api/v1/%s.json%s" entry_point path params

  let rooms t =
    api t "room/list"
    +> Http.get
    +> Json.parse
    +> rooms_of_json
end
