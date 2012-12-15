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
