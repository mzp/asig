open Asig
open AsakusaSatellite
open Base

module As = Asig.AsakusaSatellite.Make(Asig.Http.Default)

let _ =
  let uri =
    Uri.of_string Sys.argv.(1)
  in
  let room =  {
    room_id   = Sys.argv.(2);
    room_name = ""
  } in
  Lwt_unix.run @@ As.on_message uri room ~f:begin fun { name; body; room = { room_name; _ }; _ } ->
    Printf.printf "%s@%s %s\n" name room_name body;
    flush stdout
  end
