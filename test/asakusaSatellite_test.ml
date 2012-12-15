open OUnit
open Asig.Base
open Asig.AsakusaSatellite

module Stub = struct
  let response =
    ref ""

  let url =
    ref ""

  let get u =
    url := u;
    !response
end

module As = Asig.AsakusaSatellite.Make(Stub)
open As

let api =
  As.init "http://example.com/"

let auth_api =
  As.init ~api_key:"hoge" "http://example.com/"

let success = function
  | `Ok result -> result
  | `Error _ -> assert false

let assert_error = function
  | `Ok _ -> assert false
  | `Error json -> assert true

let tests = "AsakusaSatellite" >::: [
  "rooms" >:: begin fun () ->
    Stub.response :=
      "[{\"id\":\"123\", \"name\" : \"foo\"}]";
    assert_equal [ { room_id = "123"; room_name = "foo" }] @@
      success @@ rooms api;
    assert_equal "http://example.com//api/v1/room/list.json" !Stub.url
  end;
  "invalid room json" >:: begin fun () ->
    Stub.response :=
      "[{}]";
    assert_error @@ rooms api
  end;
  "private rooms" >:: begin fun () ->
    Stub.response :=
      "[]";
    assert_equal [] @@ success @@ rooms auth_api;
    assert_equal "http://example.com//api/v1/room/list.json?api_key=hoge" !Stub.url
  end;
  "message list" >:: begin fun () ->
    Stub.response :=
      "[{ \"id\" : \"123\", \"body\":\"foo\", \"name\" : \"alice\", \"screen_name\" : \"Alice\", \"room\" : { \"id\" : \"123\", \"name\" : \"room\"}}]";
    assert_equal [{
      message_id = "123";
      body       = "foo";
      name       = "alice";
      screen_name = "Alice";
      room = {
        room_id = "123";
        room_name = "room"
      }
    }] @@ success @@ messages "room_id" api;
    assert_equal "http://example.com//api/v1/message/list.json?room_id=room_id" !Stub.url
  end
]
