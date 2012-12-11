open OUnit
open Asig.Base
open Asig.AsakusaSatellite

module Stub = struct
  let response =
    ref ""

  let get _ =
    !response
end

module As = Asig.AsakusaSatellite.Make(Stub)
open As

let api =
  As.init "http://example.com/"

let tests = "AsakusaSatellite" >::: [
  "rooms" >:: fun () -> begin
    Stub.response :=
      "[{\"id\":\"123\", \"name\" : \"foo\"}]";
    assert_equal [ { room_id = "123"; room_name = "foo" }] @@
      rooms api
  end
]
