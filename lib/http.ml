let get url =
  let open Lwt in
  let open Cohttp_lwt_unix in
  let lwt =
    Client.get Uri.(of_string url) >>= function
      | None -> assert false
      | Some (_,b) -> Body.string_of_body b in
  Lwt_unix.run lwt
