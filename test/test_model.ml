open EncloseCamel
open Model
open OUnit2

let tests =
  "test suite"
  >::: [
         ( "" >:: fun _ ->
           assert_equal ~printer:print_float_tuple (440., 1.5)
             (A3.Song.convert_note ("A4", "Q.") 1.0) );
       ]

let _ = run_test_tt_main tests
