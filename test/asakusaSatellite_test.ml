open OUnit
open Asig.Base
open Asig.AsakusaSatellite

(* Lwt util *)
let assert_success expect actual =
  match Lwt_unix.run actual with
    | `Ok result -> assert_equal expect result
    | `Error _ -> assert false

let assert_error x =
  match Lwt_unix.run x with
    | `Ok _ -> assert false
    | `Error _ -> assert true

(* stub *)
module Stub = struct
  let response = ref ""
  let url      = ref ""
  let params   = ref ([] : (string * string) list)

  let get u =
    url := Uri.to_string u;
    Lwt.return @@ `Ok !response

  let post u p =
    params := p;
    url := Uri.to_string u;
    Lwt.return @@ `Ok !response

  let socket_io ~f:_ _ =
    assert false
end

let with_stub ~url ?(params = []) ~response f =
  Stub.params := [];
  Stub.response := response;
  f ();
  assert_equal ~printer:id ~msg:"url"   url    !Stub.url;
  assert_equal ~msg:"param" params !Stub.params

module As = Asig.AsakusaSatellite.Make(Stub)
open As

let api =
  Api.init "http://example.com/"

let auth_api =
  Api.init ~api_key:"hoge" "http://example.com/"

let tests = "AsakusaSatellite" >::: [
  "rooms" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/room/list.json"
      ~response:"[{\"id\":\"123\", \"name\" : \"foo\", \"nickname\" : \"nick\" }]" begin fun () ->
        assert_success [ { Room.id = "123"; name = "foo"; nickname = Some "nick" }] @@ Room.list api
      end
  end;
  "rooms without nick" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/room/list.json"
      ~response:"[{\"id\":\"123\", \"name\" : \"foo\", \"nickname\" : null }]" begin fun () ->
        assert_success [ { Room.id = "123"; name = "foo"; nickname = None }] @@ Room.list api
      end
  end;
  "invalid room json" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/room/list.json"
      ~response:"[{}]" begin fun () ->
        assert_error @@ Room.list api
      end
  end;
  "private rooms" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/room/list.json?api_key=hoge"
      ~response:"[]" begin fun () ->
        assert_success [] @@ Room.list auth_api;
      end
  end;
  "message list" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/message/list.json?room_id=room_id"
      ~response:"[{ \"id\" : \"123\", \"body\":\"foo\", \"name\" : \"alice\", \"screen_name\" : \"Alice\", \"room\" : { \"id\" : \"123\", \"name\" : \"room\", \"nickname\": null }}]" begin fun () ->
        assert_success [{
          Message.id = "123";
          body       = "foo";
          name       = "alice";
          screen_name = "Alice";
          room = {
            Room.id = "123";
            name = "room";
            nickname = None
          }
        }] @@ Message.list (Room.make "room_id") api
      end
  end;
  "message post" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/message.json"
      ~response:"{}"
      ~params:[
        "room_id", "room_id";
        "message", "hi";
        "api_key", "hoge"
      ] begin fun () ->
        assert_success () @@ Message.post (Room.make "room_id") "hi" auth_api
      end
  end;
  "info" >:: begin fun () ->
    with_stub
      ~url:"http://example.com//api/v1/service/info.json?api_key=hoge"
      ~response:"{\"message_pusher\":{\"name\":\"keima\",\"param\":{\"url\":\"url\",\"key\":\"key\"}}}"
      ~params:[]
      begin fun () ->
        assert_success {
          Service.message_pusher = {
            MessagePusher.name = "keima";
            param = {
              MessagePusher.Param.url = "url";
              key = "key"
            }
          }
        } @@ Service.info auth_api
      end;
  end
]
