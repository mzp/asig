open Asig.Base

let version =
  "0.1.0a"

let address =
  ref "0.0.0.0"

let port =
  ref "6667"

let _ =
  let open BatOptParse in
  let opt =
    OptParser.make ~version ()
  in
  OptParser.add opt
    ~short_name:'a' ~long_name:"address"
    ~help:"bind to HOST address (default: 0.0.0.0)" @@
    StdOpt.str_callback ~metavar:"HOST" (fun s -> address := s);
  OptParser.add opt
    ~short_name:'p' ~long_name:"port"
    ~help:"use PORT (default: 6667)" @@
    StdOpt.str_callback ~metavar:"PORT" (fun s -> port := s);
  OptParser.parse_argv opt

let _ =
  let open Lwt in
  Lwt_log.notice_f "Welcome to Asig %s" version >>= fun () ->
  Lwt_log.notice_f "Start Gateway at %s:%s" !address !port >>= fun () ->
  Asig.Irc.establish_server !address !port ~action:Action.action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
