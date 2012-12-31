open Base
open Lwt

module Message = struct
  type t = {
    prefix  : string option;
    command : string;
    params  : string list;
  }

  let rec of_params trail = function
    | [] ->
      ""
    | x :: xs ->
      if String.contains x ' ' && not trail then
        ":" ^ x ^ " " ^ of_params true xs
      else
        x ^ " " ^ of_params trail xs

  let to_string { prefix; command; params } =
    Printf.sprintf "%s%s %s"
      (match prefix with Some p -> ":" ^ p ^ " " | None -> "")
      command
      (of_params false params)
end

module Command = struct
end

module Reply = struct
  type t =
    | Welcome of string * string
    | YourHost
    | Created
    | MyInfo of string * string
    | PrivMsg of string * string * string

  let to_message nick = function
    | Welcome (user, host) ->
      {
        Message.prefix = None;
        command = "001";
        params = [
          nick;
          Printf.sprintf "Welcome to the Internet Relay Network %s!%s@%s" nick user host
        ] }
    | YourHost ->
      {
        Message.prefix = None;
        command = "002";
        params = [
          nick;
          "Your host is asig, running version 0.0.1"
        ] }
    | Created ->
      {
        Message.prefix = None;
        command = "003";
        params = [
          nick;
          "This server was created 2012-12-01"
        ] }
    | MyInfo (user, channel) ->
      {
        Message.prefix = None;
        command = "004";
        params = [
          nick;
          Printf.sprintf "asig 0.0.1 %s %s" user channel
        ] }
    | PrivMsg (user, channel, message) ->
      {
        Message.prefix = Some user;
        command = "PRIVMSG";
        params = [
          "#" ^ channel;
          message
        ] }
end

type privmsg = {
  prefix  : string option;
  channel : string;
  message : string
}
type t =
  | PRIVMSG of privmsg
  | NICK of string

let parse s =
  match BatString.split s " " with
    | "PRIVMSG", s ->
      let (channel, message) =
        BatString.split s " "
      in
      let channel =
        (* strip # *)
        BatString.lchop channel
      in
      let message =
        if BatString.starts_with message ":" then
          BatString.lchop message
        else
          message
      in
      Some (PRIVMSG { channel; message; prefix = None})
    | "NICK", nick ->
      Some (NICK nick)
    | _ ->
      None

let server_fun on_recv action (io, oc) =
  let _ =
    Lwt_unix.on_signal_full 13 (fun _ _ -> ())
  in
  let stream, (push : Reply.t option -> unit) =
    Lwt_stream.create () in
  let recv =
    Lwt_io.read_lines io
    +> tee (fun stream ->
      Lwt_stream.on_terminate stream (fun () -> push None))
    +> Lwt_stream.iter_s (fun line ->
      Lwt_log.notice_f "IRC %s\n" line
      >> match parse line with
        | None   -> Lwt.return ()
        | Some c -> on_recv push c)
  in
  let send () =
    stream
    +> Lwt_stream.map (Message.to_string $ Reply.to_message "mzp")
    +> Lwt_stream.iter_s (fun s ->
      Lwt_log.notice_f "--> %s\n" s
      >> Lwt_io.write_line oc s)
  in
  recv <?> send () <?> action push

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (match_lwt getaddrinfo node service
      [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] with
        | h::_ ->
          return h
        | []   ->
          raise_lwt Not_found)
      >|= fun ai -> ai.ai_addr

let establish_server host port ~on_recv ~action =
  sockaddr_of_dns host port
  >>= begin fun sc ->
    Lwt.return @@ Lwt_io.establish_server sc begin fun ch ->
      try
        Lwt.ignore_result (Lwt_unix.handle_unix_error (server_fun on_recv action) ch;
                           >> Lwt_log.notice_f "client disconnected")
      with _ ->
        print_endline "SOME EXCEPTION"
    end end
