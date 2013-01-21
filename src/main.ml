open Asig
open Base
open AsakusaSatellite
open Lwt

module As = AsakusaSatellite.Make(Http.Default)

let uri =
  Uri.of_string Sys.argv.(2)

let room_id =
  Sys.argv.(3)

(* embed parameter in realname
   (e.g. "api=some_apikey foo=bar baz=xxx")
*)
module Params = struct
  type t = (string * string) list

  let from_string s =
      BatString.nsplit s " "
    +> List.map (flip  BatString.split "=")

  let assoc =
    BatList.Exceptionless.assoc

  let (>>=) =
    BatOption.Monad.bind

  let return =
    BatOption.Monad.return
end

module AsigState = struct
  type t = {
    nick : string;
    api  : AsakusaSatellite.t option;
    asakusa_satellite : Asig.AsakusaSatellite.message option -> unit;
    irc : (string * Asig.Irc.Reply.t) option -> unit
  }

  let send_to_as { api ; _ } channel message =
    match api with
      | Some api ->
        As.post channel message api
      | None ->
        Lwt.return (`Error "AsakusaSatellite module is not initialized")

  let recv_from_as { asakusa_satellite; _ } msg =
    asakusa_satellite @@ Some msg

  let send_to_irc { irc; nick; _ } reply =
    irc @@ Some (nick, reply)

  let is_me { nick; _ } =
    (=) nick
end

module IrcAction = struct
  let welcome nick =
    let open Irc.Reply in
    [
      Welcome (nick,"localhost");
      YourHost;
      Created;
      MyInfo ("eixwy", "spknm")
    ]

  let init_api params =
    let open Params in
    assoc "api_key" params >>= (fun api_key ->
      assoc "entry" params >>= (fun entry ->
        return @@ As.init ~api_key entry))

  let on_command state = function
    | Irc.Command.PrivMsg (_, channel, message) ->
      AsigState.send_to_as state (BatString.lchop channel) message
      >>= (fun _ -> Lwt.return state)
    | Irc.Command.User (_, _, _, realname) ->
      Lwt.return { state with
        AsigState.api = init_api @@ Params.from_string realname }
    | Irc.Command.Nick nick ->
      let state =
        { state with AsigState.nick }
      in
      List.iter (AsigState.send_to_irc state) @@ welcome nick;
      Lwt.return state
    | Irc.Command.Join channel ->
      (* FIXME: use channel name to connect as *)
      let room =
        { room_id = room_id; room_name = "" }
      in
      let _ =
        As.on_message uri room ~f:(AsigState.recv_from_as state)
      in
      AsigState.send_to_irc state (Irc.Reply.Join channel);
      AsigState.send_to_irc state (Irc.Reply.Topic (channel, "Welcome to the AsakusaSatellite"));
      Lwt.return state
end

module AsAction = struct
  let cleanup str =
    Str.global_replace (Str.regexp "[\r\n]") " " str

  let on_command state { screen_name; body; _ } =
    let body =
      cleanup body
    in
    (* FIXME: use room info to detect channel *)
    AsigState.send_to_irc state @@
      if AsigState.is_me state screen_name then
        Irc.Reply.Topic ("#as", body)
      else
        Irc.Reply.PrivMsg (screen_name, "#as", body);
    Lwt.return state
end

let peek f stream =
  Lwt_stream.is_empty stream >>= (fun b ->
    if b then
      Lwt.return `Empty
    else
      Lwt_stream.next stream >>= (fun s -> Lwt.return @@ f s))

let action irc command_stream  =
  let message_stream, asakusa_satellite =
    Lwt_stream.create ()
  in
  let rec loop state =
    Lwt.pick
      [ peek (fun m -> `Msg m) message_stream
      ; peek (fun c -> `Cmd c) command_stream ]
    >>= (function
      | `Msg m ->
        AsAction.on_command state m
        >>= loop
      | `Cmd c ->
        IrcAction.on_command state c
        >>= loop
      | `Empty ->
        Lwt.return ())
  in
  loop {
    AsigState.nick = "unknown";
    api  = None;
    irc;
    asakusa_satellite
  }

let _server =
  Irc.establish_server "localhost" "8888" ~action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
