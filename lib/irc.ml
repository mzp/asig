open Base
open Lwt

type privmsg = {
  prefix  : string option;
  channel : string;
  message : string
}
type t =
  | PRIVMSG of privmsg

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
    | _ ->
      None

let to_string = function
  | PRIVMSG { prefix; channel; message } ->
    Printf.sprintf "%s PRIVMSG #%s :%s"
      (match prefix with Some p -> ":" ^ p | None -> "")
      channel
      message

let server_fun on_recv action (io, oc) =
  let recv =
    Lwt_io.read_lines io
    +> Lwt_stream.iter_s (fun line ->
      Lwt_log.notice_f "IRC %s\n" line
      >> match parse line with
        | None   -> Lwt.return ()
        | Some c -> on_recv c)
  in
  let stream, push =
    Lwt_stream.create () in
  let send () =
    stream
    +> Lwt_stream.map to_string
    +> Lwt_stream.iter_s (Lwt_io.write_line oc)
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
  >>= (fun sc ->
    Lwt.return @@ Lwt_io.establish_server sc (Lwt.ignore_result $ server_fun on_recv action))
