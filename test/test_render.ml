open OUnit2
open EncloseCamel
open EncloseCamel.Model

let test_str_render_board _ =
  let grid =
    [|
      [| Camel; Blank; Water; Wall |];
      [| Cherry; Bees; GoldenApple; LavaBucket |];
      [| Portal 0; Portal 1; Portal 2; Portal 9 |];
    |]
  in
  let board =
    {
      grid;
      walls_remaining = 10;
      bonus_walls = ref 0;
      max_score = 50;
      camel_loc = { r = 0; c = 0 };
      consumed_lava_buckets = ref [];
    }
  in
  let s = Render.str_render_board board in
  (* We expect Camel to be 'C', Blank to be 'G', Cherry to be 'R', etc. *)
  let _expected_line1 = "C G W B \n" in
  let _expected_line2 = "R E A L \n" in
  let _expected_line3 = "0 1 2 9 \n" in

  if not (String.contains s 'C') then
    assert_failure ("Expected 'C' for Camel in " ^ s);
  if not (String.contains s 'R') then
    assert_failure ("Expected 'R' for Cherry in " ^ s);
  if not (String.contains s 'E') then
    assert_failure ("Expected 'E' for Bees in " ^ s);
  if not (String.contains s 'A') then
    assert_failure ("Expected 'A' for Golden Apple in " ^ s);
  if not (String.contains s '0') then assert_failure "Expected '0' for portal 0";
  if not (String.contains s '9') then assert_failure "Expected '9' for portal 9"

let tests =
  "Render Tests" >::: [ "test_str_render_board" >:: test_str_render_board ]
