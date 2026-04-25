open OUnit2
open EncloseCamel
open Model
open Render

let in_bounds _ =
  let board =
    {
      grid = [| [| Camel; Blank |]; [| Blank; Water |] |];
      walls_remaining = 3;
    }
  in
  let result = in_bounds board {x = 0; y = 0} in
  let expected = true
  in
  assert_equal expected result

let oob_1 _ =
  let board =
    {
      grid = [| [| Camel; Blank |]; [| Blank; Water |] |];
      walls_remaining = 3;
    }
  in
  let result = Model.in_bounds board {x = -1; y = 0} in
  let expected = false
  in
  assert_equal expected result

let oob_2 _ =
  let board =
    {
      grid = [| [| Camel; Blank |]; [| Blank; Water |] |];
      walls_remaining = 3;
    }
  in
  let result = Model.in_bounds board {x = 2; y = 2} in
  let expected = false
  in
  assert_equal expected result



let tests =
  "test suite"
  >::: [
        "in bounds" >:: in_bounds;
        "out of bounds1" >:: oob_1;
        "out of bounds2" >:: oob_2;
       ]

let _ = run_test_tt_main tests
