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
        " :" ^ x ^ of_params true xs
      else
        " " ^ x ^ of_params trail xs

  let to_string { prefix; command; params } =
    Printf.sprintf "%s%s%s"
      (match prefix with Some p -> ":" ^ p ^ " " | None -> "")
      command
      (of_params false params)

  let regexp =
    Str.regexp "\\(:[A-Za-z0-9]*\\)? ?\\([A-Za-z0-9]*\\) ?\\(.*\\)"

  let group n s =
    try
      Str.matched_group n s
    with Not_found ->
      ""

  let parse s =
    if Str.string_match regexp s 0 then
      (group 1 s, group 2 s, group 3 s)
    else
      ("","","")

  let rec parse_params str =
    if str = "" then
      []
    else if BatString.starts_with str ":" then
      [ BatString.lchop str ]
    else
      let (x, xs) =
        try
          BatString.split str ~by:" "
        with Not_found ->
          str, ""
      in
      x :: parse_params xs

  let from_string s =
    let prefix, command, params =
      parse s
    in
    let prefix =
      if BatString.starts_with prefix ":" then
        Some (BatString.lchop prefix)
      else
        None
    in
    let params =
      parse_params params
    in
    { prefix; command; params }

end

module Command = struct
  open Message

  type t =
    | PrivMsg of string * string * string
    | User    of string * string * string * string
    | Nick    of string
    | Join    of string

  let from_message = function
    | { command = "PRIVMSG"; prefix; params = [channel; message] } ->
      Some (PrivMsg ((BatOption.default "" prefix), channel, message))
    | { command = "USER"   ; params = [ username; hostname; servername; realname ]; _ } ->
      Some (User (username, hostname, servername, realname))
    | { command = "NICK"   ; params = [ nick ]; _ } ->
      Some (Nick nick)
    | { command = "JOIN"   ; params = [ channel ]; _ } ->
      Some (Join channel)
    | _ ->
      None
end

module Reply = struct
  open Message

  type t =
    | Welcome of string * string
    | YourHost
    | Created
    | MyInfo  of string * string
    | Topic   of string * string
    | PrivMsg of string * string * string
    | Join    of string

  let to_message nick = function
    | Welcome (user, host) ->
      {
        prefix = None;
        command = "001";
        params = [
          nick;
          Printf.sprintf "Welcome to the Internet Relay Network %s!%s@%s" nick user host
        ] }
    | YourHost ->
      {
        prefix = None;
        command = "002";
        params = [
          nick;
          "Your host is asig, running version 0.0.1"
        ] }
    | Created ->
      {
        prefix = None;
        command = "003";
        params = [
          nick;
          "This server was created 2012-12-01"
        ] }
    | MyInfo (user, channel) ->
      {
        prefix = None;
        command = "004";
        params = [
          nick;
          Printf.sprintf "asig 0.0.1 %s %s" user channel
        ] }
    | Topic (channel, topic) ->
      {
        prefix = None;
        command = "TOPIC";
        params = [ channel; topic ] }
    | PrivMsg (user, channel, message) ->
      {
        prefix = Some user;
        command = "PRIVMSG";
        params = [
          channel;
          message
        ] }
    | Join channel ->
      {
        prefix = Some nick;
        command = "JOIN";
        params = [
          channel
        ] }
end

let server_fun action (io, oc) =
  let _ =
    Lwt_unix.on_signal_full 13 (fun _ _ -> ())
  in
  let reply_stream, reply =
    Lwt_stream.create () in
  let command_stream =
    Lwt_io.read_lines io
    +> tee (fun stream ->  Lwt_stream.on_terminate stream (fun () -> reply None))
    +> Lwt_stream.map_s (fun line ->
      Lwt_log.notice_f "IRC %s\n" line
      >> Lwt.return line)
    +> Lwt_stream.filter_map (Command.from_message $ Message.from_string)
  in
  let send () =
    reply_stream
    +> Lwt_stream.map (Message.to_string $ uncurry Reply.to_message)
    +> Lwt_stream.iter_s (fun s ->
      Lwt_log.notice_f "--> %s\n" s
      >> Lwt_io.write_line oc s)
  in
  send () <?> action reply command_stream

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (match_lwt getaddrinfo node service
      [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] with
        | h::_ ->
          return h
        | []   ->
          raise_lwt Not_found)
      >|= fun ai -> ai.ai_addr

let establish_server host port ~action =
  sockaddr_of_dns host port
  >>= begin fun sc ->
    Lwt.return @@ Lwt_io.establish_server sc begin fun ch ->
      try
        Lwt.ignore_result (Lwt_unix.handle_unix_error (server_fun action) ch;
                           >> Lwt_log.notice_f "client disconnected")
      with _ ->
        print_endline "SOME EXCEPTION"
    end end
