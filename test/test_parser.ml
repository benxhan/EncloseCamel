open OUnit2
open EncloseCamel
open EncloseCamel.Model

let test_parse_coordinate _ =
  assert_equal (Stdlib.Ok { r = 1; c = 2 }) (Parser.parse_coordinate "[1,2]");
  assert_equal
    (Stdlib.Ok { r = 10; c = 20 })
    (Parser.parse_coordinate " [ 10 , 20 ] ");
  assert_equal (Error Parser.Empty_input) (Parser.parse_coordinate "   ");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[1 2]");
  assert_equal (Error (Parser.Not_an_int "x")) (Parser.parse_coordinate "[x,2]")

let test_load_board ctx =
  let filename = "test_board.txt" in
  let oc = open_out filename in
  Printf.fprintf oc "10\n";
  Printf.fprintf oc "50\n";
  Printf.fprintf oc "C G W B\n";
  Printf.fprintf oc "R E A L\n";
  Printf.fprintf oc "0 1 2 9\n";
  close_out oc;

  let board = Parser.load_board filename in
  Sys.remove filename;

  assert_equal 10 board.walls_remaining ~msg:"walls remaining";
  assert_equal 50 board.max_score ~msg:"max score";
  assert_equal { r = 0; c = 0 } board.camel_loc ~msg:"camel loc";

  let expected_grid =
    [|
      [| Camel; Blank; Water; Wall |];
      [| Cherry; Bees; GoldenApple; LavaBucket |];
      [| Portal 0; Portal 1; Portal 2; Portal 9 |];
    |]
  in
  assert_equal expected_grid board.grid
    ~msg:"grid parsing fails or does not match"

let tests =
  "Parser Tests"
  >::: [
         "test_parse_coordinate" >:: test_parse_coordinate;
         "test_load_board" >:: test_load_board;
       ]
