(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt
open Cohttp

module CK = Cryptokit
module Req = Cohttp_lwt_unix.Request
module Res = Cohttp_lwt_unix.Response

let base64_encode str =
  let tr = CK.Base64.encode_compact_pad () in
  CK.transform_string tr str

let sha1sum str =
  let hash = CK.Hash.sha1 () in
  CK.hash_string hash str

external ($) : ('a -> 'b) -> 'a -> 'b = "%apply"
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

module Opt = struct
  let map f = function
    | Some x -> Some(f x)
    | None -> None

  let default d = function
    | Some x -> x
    | None -> d

  let unbox = function
    | Some x -> x
    | None -> raise Not_found
end

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

type opcode =
  [ `Continuation
  | `Text
  | `Binary
  | `Close
  | `Ping
  | `Pong
  | `Ctrl of int
  | `Nonctrl of int
  ]

type frame = { opcode : opcode; final : bool; content : string }

(*let string_of_opcode = function
  | `Continuation -> "continuation frame"
  | `Text         -> "text frame"
  | `Binary       -> "binary frame"
  | `Close        -> "close frame"
  | `Ping         -> "ping frame"
  | `Pong         -> "pong frame"
  | `Ctrl i       -> Printf.sprintf "control frame code %d" i
  | `Nonctrl i    -> Printf.sprintf "non-control frame code %d" i
*)
let opcode_of_int i = match i land 0xf with
  | 0                     -> `Continuation
  | 1                     -> `Text
  | 2                     -> `Binary
  | 8                     -> `Close
  | 9                     -> `Ping
  | 10                    -> `Pong
  | i when i > 2 && i < 8 -> `Nonctrl i
  | i                     -> `Ctrl i

let int_of_opcode = function
  | `Continuation -> 0
  | `Text         -> 1
  | `Binary       -> 2
  | `Close        -> 8
  | `Ping         -> 9
  | `Pong         -> 10
  | `Ctrl i       -> i
  | `Nonctrl i    -> i

let xor mask msg =
  let len =
    String.length msg in
  let msg' =
    String.make len ' ' in
  for i = 0 to len - 1 do (* masking msg to send *)
    msg'.[i] <- Char.chr $
      Char.code mask.[i mod 4] lxor Char.code msg.[i]
  done;
  msg'

let read_be nbits ic =
  let nbytes = nbits / 8 in
  let str = String.create nbytes in
  lwt () = Lwt_io.read_into_exactly ic str 0 nbytes in
  let str = Bitstring.bitstring_of_string str in
  return $
    bitmatch str with | { intval : nbits } -> intval

let write_be nbits oc intval =
  BITSTRING { intval : nbits }
      |> Bitstring.string_of_bitstring
      |> Lwt_io.write oc

let read_int16 = read_be 16
let read_int64 = read_be 64
let write_int16 = write_be 16
let write_int64 = write_be 64

let rec read_frames ic push =
  let hdr = String.create 2 in
  lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
  let hdr = Bitstring.bitstring_of_string hdr in
  let final, _rsv1, _rsv2, _rsv3, opcode, masked, length =
    bitmatch hdr with
      | { final: 1; rsv1: 1; rsv2: 1; rsv3: 1;
          opcode: 4; masked: 1; length: 7 }
        -> final, rsv1, rsv2, rsv3, opcode, masked, length in
  let opcode = opcode_of_int opcode in
  lwt payload_len = (match length with
    | i when i < 126 -> return $ Int64.of_int i
    | 126            -> read_int16 ic
    | 127            -> read_int64 ic
    | _              -> raise_lwt (Failure "bug in module Bitstring"))
      >|= Int64.to_int
  in
  let mask = String.create 4 in
  lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 4
    else return () in
  let content = String.create payload_len in
  lwt () = Lwt_io.read_into_exactly ic content 0 payload_len in
  let content = if masked then xor mask content else content in
  let () = push (Some { opcode; final; content }) in
  read_frames ic push

let rec write_frames ~masked stream oc =
  let send_frame frame =
    let mask = CK.Random.string CK.Random.secure_rng 4 in
    let len = String.length frame.content in
    let extensions = 0 in
    let opcode = int_of_opcode frame.opcode in
    let payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let bitstring = Bitstring.string_of_bitstring $
      BITSTRING {frame.final: 1; extensions: 3;
                 opcode: 4; masked : 1; payload_len: 7} in
    lwt () = Lwt_io.write oc bitstring in
    lwt () =
      (match len with
        | n when n < 126        -> return ()
        | n when n < (1 lsl 16) -> Int64.of_int n |> write_int16 oc
        | n                     -> Int64.of_int n |> write_int64 oc)
    in
    lwt content = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask frame.content else return frame.content in
    lwt () = Lwt_io.write_from_exactly oc content 0 len in
    Lwt_io.flush oc in
  Lwt_stream.next stream >>= send_frame >> write_frames ~masked stream oc

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (match_lwt getaddrinfo node service
      [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] with
        | h::_ -> return h
        | []   -> raise_lwt Not_found)
      >|= fun ai -> ai.ai_addr

let open_connection uri =
  (* Initialisation *)
  lwt _myhostname = Lwt_unix.gethostname () in
  let host       = Opt.unbox (Uri.host uri) in
  let port       = Opt.default 80 (Uri.port uri) in

  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let connect () =
    let nonce = base64_encode $ CK.Random.string CK.Random.secure_rng 16 in
    let headers =
      Header.of_list
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Protocol", "chat";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Req.make ~headers uri in
    lwt sockaddr = sockaddr_of_dns host (string_of_int port) in
    lwt ic, oc =
      Lwt_io.open_connection sockaddr in
    try_lwt
      lwt () = Req.write (fun _ _ -> return ()) req oc in
      lwt response = Res.read ic >>= function
        | Some r -> return r
        | None -> raise_lwt Not_found in
      let headers = Res.headers response in
      (assert_lwt Res.version response = `HTTP_1_1) >>
      (assert_lwt Res.status response = `Switching_protocols) >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
         $ Header.get headers "upgrade" = Some "websocket") >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
         $ Header.get headers "connection" = Some "upgrade") >>
      (assert_lwt Header.get headers "sec-websocket-accept" =
          Some (nonce ^ websocket_uuid |> sha1sum |> base64_encode)) >>
      Lwt_log.notice_f "Connected to %s\n%!" (Uri.to_string uri) >>
      return (ic, oc)
    with exn ->
      Lwt_io.close ic <&> Lwt_io.close oc >> raise_lwt exn

  in
  lwt ic, oc = connect () in
  try_lwt
    ignore_result
      (read_frames ic push_in <&> write_frames ~masked:true stream_out oc);
    return (stream_in, push_out)
  with exn -> Lwt_io.close ic <&> Lwt_io.close oc >> raise_lwt exn

let with_connection uri f =
  lwt stream_in, push_out = open_connection uri in
  f (stream_in, push_out)
