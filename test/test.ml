open OUnit

let _ =
  OUnit.run_test_tt_main ("test" >::: [
    AsakusaSatellite_test.tests;
    Irc_test.tests
  ])
