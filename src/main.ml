open Asig
open Base
open AsakusaSatellite
open Lwt

module As = AsakusaSatellite.Make(Http.Default)

let api =
  As.init ~api_key:Sys.argv.(1) "http://asakusa-satellite.org"

let uri =
  Uri.of_string Sys.argv.(2)

let room_id =
  Sys.argv.(3)

let on_recv = function
  | Irc.PRIVMSG { Irc.channel; message; _ } ->
    As.post channel message api
    >>= (fun _ -> Lwt.return ())

let action push =
  let room =  {
    room_id   = room_id;
    room_name = ""
  } in
  As.on_message uri room ~f:begin fun { screen_name; body; _  } ->
    push @@ Some (Irc.PRIVMSG {
      prefix      = Some screen_name;
      Irc.channel = "as";
      message     = body
    })
  end
  >>= (fun _ -> Lwt.return ())

let _server =
  Irc.establish_server "localhost" "8888" ~on_recv ~action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
