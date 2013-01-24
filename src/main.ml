let _server =
  Asig.Irc.establish_server "localhost" "8888" ~action:Action.action

let _ =
  Lwt_main.run (fst (Lwt.wait ()))
