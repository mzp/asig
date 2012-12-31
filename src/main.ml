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

let on_recv push = function
  | Irc.Command.PrivMsg (_, channel, message) ->
    As.post (BatString.lchop channel) message api
    >>= (fun _ -> Lwt.return ())
  | Irc.Command.Nick _ ->
    let open Irc.Reply in
    List.iter (fun s -> push (Some ("mzp", s))) [
      Welcome ("mzp","localhost");
      YourHost;
      Created;
      MyInfo ("eixwy", "spknm")
    ];
    Lwt.return ()

let action push =
  let room =  {
    room_id   = room_id;
    room_name = ""
  } in
  As.on_message uri room ~f:begin fun { screen_name; body; _  } ->
    push @@ Some ("mzp", Irc.Reply.PrivMsg (screen_name, "#as", body))
  end
  >>= (fun _ -> Lwt.return ())

let _server =
  Irc.establish_server "localhost" "8888" ~on_recv ~action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
