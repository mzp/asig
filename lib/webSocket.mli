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

(** Module [Websocket]: websocket library for Lwt *)

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages, using Lwt streams. Thus the communicating
    with the websocket server (or client) is done using an Lwt stream
    and corresponding push function.

    TODO: Currently the functions returning streams lack a way to
    close the underlying connection, causing fds to leak.
*)

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
(** Type representing websocket opcodes *)

type frame = { opcode: opcode; final: bool; content: string }
(** The type representing websocket frames *)

val sockaddr_of_dns : string -> string -> Lwt_unix.sockaddr Lwt.t
(** [sockaddr_of_dns hostname service] returns a sockaddr that
    corresponds to the hostname and service (service can be the port
    given as a string) provided as arguments. *)

val open_connection : Uri.t -> (frame Lwt_stream.t * (frame option -> unit)) Lwt.t
(** [open_connection uri] will open a connection to the given uri, and
    return a stream and a push function that can be used to send and
    receive websocket messages. The websocket messages are simply
    strings. *)

val with_connection : Uri.t ->
  (frame Lwt_stream.t * (frame option -> unit) -> 'a Lwt.t) -> 'a Lwt.t
(** Same as above except for here you provide a function that will be
    in charge of communicating with the other end, and that takes a
    stream and a push function as arguments. *)


