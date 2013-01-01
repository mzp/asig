open OUnit
open Asig.Base
open Asig.Irc

let assert_eq = assert_equal ~printer:BatStd.dump

let tests = "Irc" >::: [
  "message" >::: [
    "from_message" >::: [
      "prefix" >:: begin fun () ->
        assert_eq ":prefix CMD" @@ Message.to_string {
          Message.prefix  = Some "prefix";
          command = "CMD";
          params  = []
        }
      end;
      "without prefix" >:: begin fun () ->
        assert_eq "CMD" @@ Message.to_string {
          Message.prefix = None;
          command = "CMD";
          params  = []
        }
      end;
      "params" >:: begin fun () ->
        assert_eq "CMD a b c" @@ Message.to_string {
          Message.prefix = None;
          command = "CMD";
          params  = ["a"; "b"; "c"]
        }
      end;
      "trail params" >:: begin fun () ->
        assert_eq "CMD a b :foo bar baz" @@ Message.to_string {
          Message.prefix = None;
          command = "CMD";
          params  = ["a"; "b"; "foo bar"; "baz"]
        }
      end
    ]
  ];
  "command" >::: [
    "PRIVMSG" >:: begin fun () ->
      assert_eq (Some (Command.PrivMsg ("foo","#as","body"))) @@ Command.from_message {
        Message.prefix = Some "foo";
        command = "PRIVMSG";
        params  = [ "#as"; "body" ]
      }
    end;
    "NICK" >:: begin fun () ->
      assert_eq (Some (Command.Nick "john")) @@ Command.from_message {
        Message.prefix = None;
        command = "NICK";
        params  = [ "john" ]
      }
    end;
    "otherwise" >:: begin fun () ->
      assert_eq None @@ Command.from_message {
        Message.prefix = None;
        command = "UNKNOWN";
        params  = []
      }
    end
  ];
  "reply" >:::[
    "welcome" >:: begin fun () ->
      assert_eq {
        Message.prefix = None;
        command = "001";
        params = [ "john"; "Welcome to the Internet Relay Network john!john@localhost"]
      } @@ Reply.to_message "john" (Reply.Welcome ("john", "localhost"))
    end
  ]
]
