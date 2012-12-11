open Asig.Base

module As = Asig.AsakusaSatellite.Make(Asig.Http)

let _ =
  As.init "http://asakusa-satellite.org"
  +> As.rooms
  +> List.map (fun { Asig.AsakusaSatellite.room_name } -> room_name )
  +> List.iter print_endline
