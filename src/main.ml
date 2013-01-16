open Asig
open Base
open AsakusaSatellite
open Lwt

module As = AsakusaSatellite.Make(Http.Default)

let uri =
  Uri.of_string Sys.argv.(2)

let room_id =
  Sys.argv.(3)

type t = {
  nick : string;
  api  : AsakusaSatellite.t option
}

let parse s =
  BatString.nsplit s " "
  +> List.map (flip  BatString.split "=")

let on_recv push state = function
  | Irc.Command.PrivMsg (_, channel, message) ->
    begin match state.api with
      | Some api ->
        As.post (BatString.lchop channel) message api
      | None ->
        Lwt.return (`Error "AsakusaSatellite module is not initialized")
    end >>= (fun _ -> Lwt.return state)
  | Irc.Command.User (_, _, _, realname) ->
    (* embed parameter in realname
       (e.g. "api=some_apikey foo=bar baz=xxx")
    *)
    let params =
      parse realname
    in
    let api =
      let open BatOption.Monad in
      let module L = BatList.Exceptionless in
      let (>>=)    = bind in
      L.assoc "api_key" params >>= (fun api_key ->
        L.assoc "entry" params >>= (fun entry ->
          return @@ As.init ~api_key entry))
    in
    Lwt.return { state with api }
  | Irc.Command.Nick nick ->
    let open Irc.Reply in
    List.iter (fun s -> push (Some (nick, s))) [
      Welcome (nick,"localhost");
      YourHost;
      Created;
      MyInfo ("eixwy", "spknm")
    ];
    Lwt.return { state with nick }
  | Irc.Command.Join channel ->
    push @@ Some (state.nick, Irc.Reply.Topic (channel, "Welcome to the AsakusaSatellite"));
    Lwt.return state

let as_stream () =
  let stream, push =
    Lwt_stream.create ()
  in
  let room =
    { room_id = room_id; room_name = "" }
  in
  stream, As.on_message uri room ~f:(fun msg -> push @@ Some msg)

let action push command_stream  =
  let message_stream, as_loop =
    as_stream ()
  in
  let rec loop state =
    Lwt.pick [ Lwt_stream.next message_stream     >>= (fun m -> Lwt.return (`Msg m))
             ; Lwt_stream.next command_stream     >>= (fun m -> Lwt.return (`Cmd m))
             ; Lwt_stream.is_empty command_stream >>= (fun _ -> Lwt.return `Exit)]
    >>= (function
      | `Msg { screen_name; body; _ } ->
        Lwt.wrap (fun () ->
          let body =
            Str.global_replace (Str.regexp "[\r\n]") " " body
          in
          if screen_name = state.nick then
            push @@ Some (state.nick, Irc.Reply.Topic ("#as", body))
          else
            push @@ Some (state.nick, Irc.Reply.PrivMsg (screen_name, "#as", body)))
        >>= (fun () -> loop state)
      | `Cmd c ->
        on_recv push state c
        >>= loop
      | `Exit ->
        Lwt.return ())
  in
  (loop { nick = "unknown"; api = None }) <&> (as_loop >>= (fun _ -> Lwt.return ()))

let _server =
  Irc.establish_server "localhost" "8888" ~action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
