open Base
open Cohttp_lwt_unix

type 'a result = ('a, string) ElMonad.t

let (>>=) = Lwt.bind

type socket_io_event =
  | Connection
  | Json of Tiny_json.Json.t

module type S = sig
  val get       : Uri.t -> string result
  val post      : Uri.t -> (string * string) list -> string result
  val socket_io : f:((socket_io_event -> unit) -> socket_io_event -> unit) -> Uri.t -> unit result
end

let response = function
  | None ->
    Lwt.return (`Error "connection error")
  | Some (response,b) ->
    let status =
      Response.status response
    in
    if Cohttp.Code.is_success @@ Cohttp.Code.code_of_status status then
      Body.string_of_body b
      +> Lwt.map (fun s -> `Ok s)
    else
      Lwt.return (`Error (Cohttp.Code.string_of_status status))

let split3 s by =
  let (a,b) =
    BatString.split s ~by in
  let (b,c) =
    BatString.split b ~by in
  (a,b,c)

module Default = struct
  let get uri =
    Client.get uri >>= response

  let post uri params =
    let params =
      Cohttp.Header.of_list params
    in
    Client.post_form ~params uri >>= response

  let handler f (stream, push) =
    let send_data = function
      | Json json ->
        push (Some {WebSocket.opcode=`Text; final=true; content="5:::"^JsonUtil.string_of_json json})
      | Connection ->
        ()
    in
    let rec write_fun () =
      Lwt_stream.next stream
      >>= fun { WebSocket.content; _ } -> begin
        match BatString.split content ~by:":" with
          | ("5", content) ->
            let (_,_,json) =
              split3 content ":"
            in
            Lwt.return @@ f send_data @@ Json (Tiny_json.Json.parse json)
          | ("1",_) ->
            Lwt.wrap (fun () -> f send_data Connection)
          | ("2",_) ->
            Lwt.wrap (fun () ->
              push (Some {WebSocket.opcode=`Text; final=true; content="2::"}))
          | _ ->
            Lwt.return @@ print_endline ("-->" ^ content)
      end
      >>= write_fun
    in
    write_fun ()

  let socket_io ~f uri =
    (* connect to entry point *)
    get uri
    (* get sid from response *)
    +> ElMonad.lift (fun s -> match BatString.nsplit s ~by:":" with
      | sid :: heartbeat :: _ -> `Ok (sid, heartbeat)
      | _ -> `Error "socket io handshake error")
    (* create url for websocket *)
    +> ElMonad.fmap (fun (sid, _) ->
      Uri.make ~scheme:"ws" ?host:(Uri.host uri) ?port:(Uri.port uri)
        ~path:(Printf.sprintf "%swebsocket/%s" (Uri.path uri) sid)
        ())
    (* handshake with websocket *)
    >>= (function
      | `Ok uri -> WebSocket.with_connection uri (handler f)
      | `Error _ as e -> Lwt.return e)

end
