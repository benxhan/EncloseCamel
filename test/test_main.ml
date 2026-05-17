open OUnit2

let all_tests =
  "EncloseCamel full test suite"
  >::: [
         Test_enclosecamel.all_tests;
         Test_model.all_tests;
         Test_parser.all_tests;
         Test_render.all_tests;
         Test_extra_coverage.all_tests;
       ]

let () = run_test_tt_main all_tests

