open Asig.Base

module As = Asig.AsakusaSatellite.Make(Asig.Http)

let api =
  if Array.length Sys.argv < 2 then
    As.init "http://asakusa-satellite.org"
  else
    As.init ~api_key:Sys.argv.(1) "http://asakusa-satellite.org"

let _ =
  let open Meta_conv.Types.Result in
  As.rooms api
  +> fmap (List.map (fun { Asig.AsakusaSatellite.room_id; room_name } ->
    Printf.sprintf "%s  %s" room_id room_name))
  +> fmap (List.iter print_endline)

let _ =
  print_endline "------------------------------"

let _ =
  let open Meta_conv.Types.Result in
  As.messages Sys.argv.(2) api

  +> fmap (List.map (fun { Asig.AsakusaSatellite.message_id; name; body; _ } ->
    Printf.sprintf "%s  %s %s" message_id name body))
  +> fmap (List.iter print_endline)
