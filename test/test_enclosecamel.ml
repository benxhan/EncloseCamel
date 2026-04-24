open OUnit2
open EncloseCamel
open Model
open Render

let test_render_board _ =
  let board1 =
    { grid = [| [| Camel; Blank |]; [| Blank; Water |] |]; walls_remaining = 3 }
  in
  let result = str_render_board board1 in
  let expected =
    "C G \n\
     G W \n\
     Coordinates of the Camel are (_, _)\n\
     Number of walls remaining: 3"
  in
  assert_equal expected result

let test_render_empty _ =
  let board = { grid = [| [||] |]; walls_remaining = 3 } in
  let result = str_render_board board in
  let expected =
    "\nCoordinates of the Camel are (_, _)\nNumber of walls remaining: 3"
  in
  assert_equal expected result

let tests =
  "test suite"
  >::: [
         "render_board basic" >:: test_render_board;
         "render_board empty" >:: test_render_empty;
       ]

let _ = run_test_tt_main tests
