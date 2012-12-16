module As = Asig.AsakusaSatellite.Make(Asig.Http)
(*
let cat_fun (stream, _) =
  let open Lwt in
  let open WebSocket in
  let rec write_fun () =
    Lwt_stream.next stream
      >>= fun {content; _ } -> print_endline "hi"; Lwt_io.printl content
      >>= write_fun in
  write_fun ()

let () =
  Lwt_main.run @@ WebSocket.with_connection (Uri.of_string "ws://localhost:8080/") cat_fun
*)

let _ =
  let open Asig.AsakusaSatellite in
  As.on_message (fun { name; body; room = { room_name; _ }; _ } ->
    Printf.printf "%s@%s %s\n" name room_name body;
    flush stdout) Sys.argv.(1)
(*let _ =
  Asig.Http.socket_io (fun _ -> ()) "http://keima.c.node-ninja.com/socket.io/1/?app=503a09aba3d38c2944000001&t=1355636614357"
*)
