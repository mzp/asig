open Base
open Lwt
open Cohttp_lwt_unix

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


let get url =
  (Client.get Uri.(of_string url) >>= response)
  +> Lwt_unix.run

let post url params =
  let params =
    Cohttp.Header.of_list params
  in
  (Client.post_form ~params Uri.(of_string url) >>= response)
  +> Lwt_unix.run

let map f x =
  Lwt.map (Either.fmap f) x

let split3 s sep =
  let (a,b) =
    BatString.split s sep in
  let (b,c) =
    BatString.split b sep in
  (a,b,c)

let handler f (stream, push) =
  let rec write_fun () =
    Lwt_stream.next stream
    >>= fun { WebSocket.content; _ } -> begin
      match BatString.split content ":" with
        | ("5", content) ->
          let (_,_,json) =
            split3 content ":"
          in
          Lwt.return @@ f (Tiny_json.Json.parse json)
        | ("1",_) ->
          wrap (fun () ->
            print_endline "subs";
            push (Some {WebSocket.opcode=`Text; final=true; content="5:::{ \"name\":\"subscribe\", \"args\":[\"as-50d07248489e0117e6000003\"]}"}))
        | _ ->
          Lwt.return @@ print_endline ("-->" ^ content)
    end
    >>= write_fun
  in
  write_fun ()

let socket_io f url =
  let x =
    (Client.get Uri.(of_string url) >>= response)
    +> Lwt.map (fun x -> Either.bind x (fun s -> match BatString.nsplit s ":" with
      | sid :: heartbeat :: _ -> `Ok (sid, heartbeat)
      | _ -> `Error "socket io handshake error"))
    +> map (fun (sid, heartbeat) ->
      (Uri.of_string @@ Printf.sprintf "ws://localhost:3001/socket.io/1/websocket/%s" sid), heartbeat)
    >>= (function
      | `Ok (uri, _) -> WebSocket.with_connection uri (handler f)
      | `Error _ -> assert false)
  in
  Lwt_unix.run x
