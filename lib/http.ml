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
