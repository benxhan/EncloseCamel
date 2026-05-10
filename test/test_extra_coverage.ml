open OUnit2
open EncloseCamel
open EncloseCamel.Model

let assert_raises_failure msg f =
  try
    ignore (f ());
    assert_failure msg
  with Failure _ -> ()

let write_temp_board lines =
  let filename = Filename.temp_file "ec_board_" ".txt" in
  let oc = open_out filename in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines;
  close_out oc;
  filename

let cleanup filename = if Sys.file_exists filename then Sys.remove filename

let test_properties_of_all_tiles _ =
  let check tile points walkable file_char =
    let p = properties_of tile in
    assert_equal points p.points ~printer:string_of_int;
    assert_equal walkable p.walkable ~printer:string_of_bool;
    assert_equal file_char p.file_char ~printer:(String.make 1)
  in
  check Blank 1 true 'G';
  check Cherry 5 true 'R';
  check Bees (-5) true 'E';
  check GoldenApple 10 true 'A';
  check (Portal 3) 1 true 'P';
  check LavaBucket 1 true 'L';
  check Camel 1 true 'C';
  check Water 0 false 'W';
  check Wall 0 false 'B';
  check Mouse 0 false 'M';
  check Cheese 0 true 'Z'

let test_base_tiles _ =
  assert_bool "base_tiles should include Camel" (List.mem Camel base_tiles);
  assert_bool "base_tiles should include Water" (List.mem Water base_tiles);
  assert_bool "base_tiles should include Wall" (List.mem Wall base_tiles);
  assert_bool "base_tiles should include Blank" (List.mem Blank base_tiles);
  assert_bool "base_tiles should include Cherry" (List.mem Cherry base_tiles);
  assert_bool "base_tiles should include Bees" (List.mem Bees base_tiles);
  assert_bool "base_tiles should include GoldenApple"
    (List.mem GoldenApple base_tiles);
  assert_bool "base_tiles should include LavaBucket"
    (List.mem LavaBucket base_tiles);
  assert_bool "base_tiles should include Mouse" (List.mem Mouse base_tiles);
  assert_bool "base_tiles should include Cheese" (List.mem Cheese base_tiles)

let test_init_failure_cases _ =
  assert_raises_failure "bad dimensions should fail" (fun () ->
      init ~width:0 ~height:3 ~camel:{ r = 0; c = 0 } ~water:[] ~lava_buckets:[]
        ~walls_available:1 ~max_score:0);
  assert_raises_failure "camel out of bounds should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 5; c = 0 } ~water:[] ~lava_buckets:[]
        ~walls_available:1 ~max_score:0);
  assert_raises_failure "water out of bounds should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 0; c = 0 }
        ~water:[ { r = 4; c = 0 } ]
        ~lava_buckets:[] ~walls_available:1 ~max_score:0);
  assert_raises_failure "duplicate water should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 0; c = 0 }
        ~water:[ { r = 1; c = 1 }; { r = 1; c = 1 } ]
        ~lava_buckets:[] ~walls_available:1 ~max_score:0);
  assert_raises_failure "duplicate lava buckets should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 0; c = 0 } ~water:[]
        ~lava_buckets:[ { r = 1; c = 1 }; { r = 1; c = 1 } ]
        ~walls_available:1 ~max_score:0);
  assert_raises_failure "camel on water should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 1; c = 1 }
        ~water:[ { r = 1; c = 1 } ]
        ~lava_buckets:[] ~walls_available:1 ~max_score:0)

let test_reset_branch_of_place_wall _ =
  let b =
    init ~width:3 ~height:3 ~camel:{ r = 1; c = 1 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Wall;
  let b = { b with walls_remaining = 0; needs_reset = true } in
  match place_wall b { r = 2; c = 2 } with
  | Stdlib.Ok nb ->
      assert_equal 5 nb.walls_remaining ~printer:string_of_int;
      assert_equal false nb.needs_reset ~printer:string_of_bool;
      assert_equal Blank (get_tile nb { r = 0; c = 0 })
  | Error _ -> assert_failure "reset branch should return Ok"

let test_place_wall_mouse_no_cheese _ =
  let b =
    init ~width:4 ~height:4 ~camel:{ r = 3; c = 3 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  match place_wall b { r = 1; c = 1 } with
  | Stdlib.Ok nb ->
      assert_equal Wall (get_tile nb { r = 1; c = 1 });
      assert_equal Mouse (get_tile nb { r = 0; c = 0 })
  | Error _ -> assert_failure "placing wall should succeed"

let test_place_wall_mouse_moves_to_blank _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 3 } Cheese;
  match place_wall b { r = 3; c = 3 } with
  | Stdlib.Ok nb ->
      assert_equal Blank (get_tile nb { r = 0; c = 0 });
      assert_equal Mouse (get_tile nb { r = 0; c = 1 });
      assert_equal Cheese (get_tile nb { r = 0; c = 3 })
  | Error _ -> assert_failure "placing wall should succeed"

let test_place_wall_mouse_blocked_path _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 2 } Cheese;
  set_tile b { r = 0; c = 1 } Wall;
  set_tile b { r = 1; c = 0 } Wall;
  set_tile b { r = 1; c = 1 } Wall;
  match place_wall b { r = 3; c = 3 } with
  | Stdlib.Ok nb ->
      assert_equal Mouse (get_tile nb { r = 0; c = 0 });
      assert_equal Cheese (get_tile nb { r = 0; c = 2 })
  | Error _ -> assert_failure "placing wall should still succeed"

let test_remove_wall_mouse_moves _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 3 } Cheese;
  set_tile b { r = 3; c = 3 } Wall;
  match place_wall b { r = 3; c = 3 } with
  | Stdlib.Ok nb ->
      assert_equal Blank (get_tile nb { r = 3; c = 3 });
      assert_equal Blank (get_tile nb { r = 0; c = 0 });
      assert_equal Mouse (get_tile nb { r = 0; c = 1 })
  | Error _ -> assert_failure "removing wall should succeed"

let test_parse_coordinate_extra_errors _ =
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[1,]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[,2]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[1,2,3]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "1,2");
  assert_equal (Error (Parser.Not_an_int "y")) (Parser.parse_coordinate "[1,y]");
  assert_equal (Stdlib.Ok { r = -1; c = 2 }) (Parser.parse_coordinate "[-1,2]")

let test_parse_error_to_string _ =
  assert_bool "empty message"
    (String.length (Parser.parse_error_to_string Parser.Empty_input) > 0);
  assert_bool "bad format message"
    (String.contains (Parser.parse_error_to_string Parser.Bad_format) '[');
  assert_bool "not int message contains token"
    (String.contains
       (Parser.parse_error_to_string (Parser.Not_an_int "abc"))
       'a')

let test_load_board_failure_cases _ =
  let run_bad lines =
    let filename = write_temp_board lines in
    try
      ignore (Parser.load_board filename);
      cleanup filename;
      assert_failure "expected load_board failure"
    with Failure _ -> cleanup filename
  in
  run_bad [];
  run_bad [ "not_an_int" ];
  run_bad [ "10" ];
  run_bad [ "10"; "not_score"; "C" ];
  run_bad [ "10"; "20" ];
  run_bad [ "10"; "20"; "C"; "GG" ];
  run_bad [ "10"; "20"; "C X" ];
  run_bad [ "10"; "20"; "G G"; "G G" ];
  run_bad [ "10"; "20"; "C C"; "G G" ]

let test_load_board_tip_marker_without_text _ =
  let filename = write_temp_board [ "3"; "10"; "C G"; "G G"; "tip" ] in
  let board = Parser.load_board filename in
  cleanup filename;
  assert_equal None board.tip

let test_load_board_lowercase_tip _ =
  let filename =
    write_temp_board [ "3"; "10"; "C G"; "G G"; "tip"; "lowercase works" ]
  in
  let board = Parser.load_board filename in
  cleanup filename;
  assert_equal (Some "lowercase works") board.tip

let test_render_string_exact _ =
  let grid =
    [|
      [| Camel; Blank; Water; Wall |];
      [| Cherry; Bees; GoldenApple; LavaBucket |];
      [| Portal 0; Portal 1; Mouse; Cheese |];
    |]
  in
  let board =
    {
      grid;
      walls_remaining = 7;
      max_score = 20;
      camel_loc = { r = 0; c = 0 };
      tip = None;
      initial_grid = Array.map Array.copy grid;
      initial_walls = 7;
      needs_reset = false;
    }
  in
  let expected =
    "C G W B \n\
     R E A L \n\
     0 1 M Z \n\
     Coordinates of the Camel are (_, _)\n\
     Number of walls remaining: 7"
  in
  assert_equal expected (Render.str_render_board board)

let test_render_board_open_and_enclosed _ =
  let open_board =
    init ~width:4 ~height:4 ~camel:{ r = 1; c = 1 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile open_board { r = 0; c = 0 } Blank;
  set_tile open_board { r = 0; c = 1 } Water;
  set_tile open_board { r = 0; c = 2 } Wall;
  set_tile open_board { r = 0; c = 3 } (Portal 2);
  set_tile open_board { r = 1; c = 0 } Cherry;
  set_tile open_board { r = 1; c = 2 } Bees;
  set_tile open_board { r = 1; c = 3 } GoldenApple;
  set_tile open_board { r = 2; c = 0 } LavaBucket;
  set_tile open_board { r = 2; c = 1 } Mouse;
  set_tile open_board { r = 2; c = 2 } Cheese;
  Render.render_board open_board;

  let enclosed_board =
    init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 } ~water:[] ~lava_buckets:[]
      ~walls_available:10 ~max_score:0
  in
  set_tile enclosed_board { r = 1; c = 2 } Wall;
  set_tile enclosed_board { r = 3; c = 2 } Wall;
  set_tile enclosed_board { r = 2; c = 1 } Wall;
  set_tile enclosed_board { r = 2; c = 3 } Wall;
  set_tile enclosed_board { r = 0; c = 0 } Blank;
  Render.render_board enclosed_board;
  assert_bool "render_board did not raise" true

let test_print_place_result _ =
  Render.print_place_result Out_of_bounds;
  Render.print_place_result Occupied;
  Render.print_place_result Ok;
  assert_bool "print_place_result did not raise" true

let test_render_gui_size_helpers _ =
  assert_equal 64 Render.base_gui_tile_px ~printer:string_of_int;
  assert_equal 64 Render.tile_size ~printer:string_of_int;
  assert_equal 96 Render.info_panel_height ~printer:string_of_int;

  Render.set_gui_tile_px 32;
  assert_equal 32 (Render.gui_tile_px ()) ~printer:string_of_int;

  Render.set_gui_tile_px 0;
  assert_equal 1 (Render.gui_tile_px ()) ~printer:string_of_int;

  Render.set_gui_tile_px 1000;
  assert_equal 64 (Render.gui_tile_px ()) ~printer:string_of_int;

  Render.set_gui_tile_px 64;
  assert_equal 64 (Render.gui_tile_px ()) ~printer:string_of_int

let test_load_board_with_every_tile _ =
  let filename =
    write_temp_board
      [
        "12";
        "99";
        "C W B G";
        "R E A L";
        "M Z 0 9";
        "tips";
        "This is a multi-line tip.";
        "Second line.";
      ]
  in
  let board = Parser.load_board filename in
  cleanup filename;

  assert_equal 12 board.walls_remaining ~printer:string_of_int;
  assert_equal 99 board.max_score ~printer:string_of_int;
  assert_equal { r = 0; c = 0 } board.camel_loc;
  assert_equal Camel (get_tile board { r = 0; c = 0 });
  assert_equal Water (get_tile board { r = 0; c = 1 });
  assert_equal Wall (get_tile board { r = 0; c = 2 });
  assert_equal Blank (get_tile board { r = 0; c = 3 });
  assert_equal Cherry (get_tile board { r = 1; c = 0 });
  assert_equal Bees (get_tile board { r = 1; c = 1 });
  assert_equal GoldenApple (get_tile board { r = 1; c = 2 });
  assert_equal LavaBucket (get_tile board { r = 1; c = 3 });
  assert_equal Mouse (get_tile board { r = 2; c = 0 });
  assert_equal Cheese (get_tile board { r = 2; c = 1 });
  assert_equal (Portal 0) (get_tile board { r = 2; c = 2 });
  assert_equal (Portal 9) (get_tile board { r = 2; c = 3 });
  assert_equal (Some "This is a multi-line tip.\nSecond line.") board.tip

let test_load_board_ignores_blank_grid_lines _ =
  let filename =
    write_temp_board [ "5"; "10"; ""; "C G"; ""; "G W"; ""; "TIP"; "hint" ]
  in
  let board = Parser.load_board filename in
  cleanup filename;

  assert_equal 2 (Array.length board.grid) ~printer:string_of_int;
  assert_equal 2 (Array.length board.grid.(0)) ~printer:string_of_int;
  assert_equal Camel (get_tile board { r = 0; c = 0 });
  assert_equal Blank (get_tile board { r = 0; c = 1 });
  assert_equal Blank (get_tile board { r = 1; c = 0 });
  assert_equal Water (get_tile board { r = 1; c = 1 });
  assert_equal (Some "hint") board.tip

let test_place_wall_mouse_moves_next_to_cheese_no_reset _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 2 } Cheese;

  match place_wall b { r = 3; c = 3 } with
  | Stdlib.Ok nb ->
      assert_equal false nb.needs_reset ~printer:string_of_bool;
      assert_equal Blank (get_tile nb { r = 0; c = 0 });
      assert_equal Mouse (get_tile nb { r = 0; c = 1 });
      assert_equal Cheese (get_tile nb { r = 0; c = 2 })
  | Error _ -> assert_failure "placing wall should succeed"

let test_remove_wall_mouse_moves_next_to_cheese_no_reset _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 2 } Cheese;
  set_tile b { r = 3; c = 3 } Wall;

  match place_wall b { r = 3; c = 3 } with
  | Stdlib.Ok nb ->
      assert_equal false nb.needs_reset ~printer:string_of_bool;
      assert_equal Blank (get_tile nb { r = 0; c = 0 });
      assert_equal Mouse (get_tile nb { r = 0; c = 1 });
      assert_equal Cheese (get_tile nb { r = 0; c = 2 })
  | Error _ -> assert_failure "removing wall should succeed"

let test_reachable_portal_same_id_already_visited _ =
  let b =
    init ~width:7 ~height:7 ~camel:{ r = 3; c = 3 } ~water:[] ~lava_buckets:[]
      ~walls_available:20 ~max_score:0
  in

  let walls =
    [
      { r = 2; c = 2 };
      { r = 2; c = 3 };
      { r = 2; c = 4 };
      { r = 3; c = 2 };
      { r = 3; c = 5 };
      { r = 4; c = 2 };
      { r = 4; c = 3 };
      { r = 4; c = 4 };
      { r = 4; c = 5 };
      { r = 2; c = 5 };
    ]
  in
  List.iter (fun c -> set_tile b c Wall) walls;

  set_tile b { r = 3; c = 4 } (Portal 2);

  match reachable_from_camel b with
  | Enclosed { score; bonus_walls; _ } ->
      assert_bool "score should include reachable portal" (score >= 2);
      assert_equal 0 bonus_walls ~printer:string_of_int
  | Open -> assert_failure "expected enclosed"

let test_render_board_many_tiles_open _ =
  let grid =
    [|
      [| Camel; Water; Wall; Blank |];
      [| Cherry; Bees; GoldenApple; LavaBucket |];
      [| Portal 3; Mouse; Cheese; Blank |];
      [| Blank; Blank; Blank; Blank |];
    |]
  in
  let board =
    {
      grid;
      walls_remaining = 8;
      max_score = 100;
      camel_loc = { r = 0; c = 0 };
      tip = None;
      initial_grid = Array.map Array.copy grid;
      initial_walls = 8;
      needs_reset = false;
    }
  in
  Render.render_board board;
  assert_bool "render_board open board did not raise" true

let test_render_board_many_tiles_enclosed _ =
  let b =
    init ~width:6 ~height:6 ~camel:{ r = 2; c = 2 } ~water:[] ~lava_buckets:[]
      ~walls_available:20 ~max_score:100
  in

  let walls =
    [
      { r = 1; c = 1 };
      { r = 1; c = 2 };
      { r = 1; c = 3 };
      { r = 1; c = 4 };
      { r = 2; c = 1 };
      { r = 2; c = 4 };
      { r = 3; c = 1 };
      { r = 3; c = 2 };
      { r = 3; c = 3 };
      { r = 3; c = 4 };
    ]
  in
  List.iter (fun c -> set_tile b c Wall) walls;
  set_tile b { r = 2; c = 3 } Blank;
  set_tile b { r = 0; c = 0 } Cherry;
  set_tile b { r = 0; c = 1 } Bees;
  set_tile b { r = 0; c = 2 } GoldenApple;
  set_tile b { r = 0; c = 3 } LavaBucket;
  set_tile b { r = 0; c = 4 } Mouse;
  set_tile b { r = 0; c = 5 } Cheese;
  set_tile b { r = 5; c = 5 } (Portal 4);

  Render.render_board b;
  assert_bool "render_board enclosed board did not raise" true

let test_init_lava_bucket_out_of_bounds _ =
  assert_raises_failure "lava bucket out of bounds should fail" (fun () ->
      init ~width:3 ~height:3 ~camel:{ r = 0; c = 0 } ~water:[]
        ~lava_buckets:[ { r = 5; c = 1 } ]
        ~walls_available:1 ~max_score:0)

let test_check_coord_placement_more_tiles _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Cherry;
  set_tile b { r = 0; c = 1 } Bees;
  set_tile b { r = 0; c = 2 } GoldenApple;
  set_tile b { r = 0; c = 3 } LavaBucket;
  set_tile b { r = 0; c = 4 } Mouse;
  set_tile b { r = 1; c = 0 } Cheese;
  set_tile b { r = 1; c = 1 } (Portal 7);

  assert_equal Occupied (check_coord_placement b { r = 0; c = 0 });
  assert_equal Occupied (check_coord_placement b { r = 0; c = 1 });
  assert_equal Occupied (check_coord_placement b { r = 0; c = 2 });
  assert_equal Occupied (check_coord_placement b { r = 0; c = 3 });
  assert_equal Occupied (check_coord_placement b { r = 0; c = 4 });
  assert_equal Occupied (check_coord_placement b { r = 1; c = 0 });
  assert_equal Occupied (check_coord_placement b { r = 1; c = 1 });
  assert_equal Ok (check_coord_placement b { r = 4; c = 4 })

let test_neighbors4_all_corners _ =
  let b =
    init ~width:3 ~height:3 ~camel:{ r = 1; c = 1 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  let sort cs =
    List.sort
      (fun a b -> if a.r = b.r then compare a.c b.c else compare a.r b.r)
      cs
  in
  let assert_coords exp got = assert_equal (sort exp) (sort got) in

  assert_coords
    [ { r = 0; c = 1 }; { r = 1; c = 0 } ]
    (neighbors4 b { r = 0; c = 0 });
  assert_coords
    [ { r = 0; c = 1 }; { r = 1; c = 2 } ]
    (neighbors4 b { r = 0; c = 2 });
  assert_coords
    [ { r = 1; c = 0 }; { r = 2; c = 1 } ]
    (neighbors4 b { r = 2; c = 0 });
  assert_coords
    [ { r = 1; c = 2 }; { r = 2; c = 1 } ]
    (neighbors4 b { r = 2; c = 2 })

let test_next_mouse_step_start_adjacent_returns_none _ =
  let b =
    init ~width:4 ~height:4 ~camel:{ r = 3; c = 3 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  assert_equal None (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 1 });
  assert_equal None (next_mouse_step b { r = 0; c = 0 } { r = 1; c = 0 })

let test_next_mouse_step_avoids_nonwalkable_tiles _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Mouse;
  set_tile b { r = 0; c = 4 } Cheese;
  set_tile b { r = 0; c = 1 } Water;
  set_tile b { r = 1; c = 0 } Cherry;
  (* Cherry is not walkable for mouse pathfinding, so mouse should not step
     there. The path is blocked at start. *)
  assert_equal None (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 4 })

let test_place_wall_on_special_tiles_fails _ =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 } ~water:[] ~lava_buckets:[]
      ~walls_available:5 ~max_score:0
  in
  set_tile b { r = 0; c = 0 } Cherry;
  set_tile b { r = 0; c = 1 } Bees;
  set_tile b { r = 0; c = 2 } GoldenApple;
  set_tile b { r = 0; c = 3 } LavaBucket;
  set_tile b { r = 0; c = 4 } Mouse;
  set_tile b { r = 1; c = 0 } Cheese;
  set_tile b { r = 1; c = 1 } (Portal 3);

  let expect_occupied coord =
    match place_wall b coord with
    | Error Occupied -> ()
    | _ -> assert_failure "expected Error Occupied"
  in

  expect_occupied { r = 0; c = 0 };
  expect_occupied { r = 0; c = 1 };
  expect_occupied { r = 0; c = 2 };
  expect_occupied { r = 0; c = 3 };
  expect_occupied { r = 0; c = 4 };
  expect_occupied { r = 1; c = 0 };
  expect_occupied { r = 1; c = 1 }

let test_parser_file_not_found _ =
  try
    ignore (Parser.load_board "this_file_should_not_exist_12345.txt");
    assert_failure "expected Sys_error for missing file"
  with Sys_error _ -> ()

let test_parse_coordinate_short_bad_formats _ =
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[1]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[,]");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "[1,2");
  assert_equal (Error Parser.Bad_format) (Parser.parse_coordinate "1,2]")

let test_render_unload_gui_textures_none _ =
  (* Covers the None branch. Safe even without assets. *)
  Render.unload_gui_textures ();
  assert_bool "unload_gui_textures with no textures did not raise" true

let test_init_width_bug_case _ =
  assert_raises_failure "valid rectangular-board coord currently fails"
    (fun () ->
      init ~width:5 ~height:3 ~camel:{ r = 0; c = 4 } ~water:[] ~lava_buckets:[]
        ~walls_available:1 ~max_score:0)

let all_tests =
  "extra coverage tests"
  >::: [
         "properties_of_all_tiles" >:: test_properties_of_all_tiles;
         "base_tiles" >:: test_base_tiles;
         "init_failure_cases" >:: test_init_failure_cases;
         "reset_branch_of_place_wall" >:: test_reset_branch_of_place_wall;
         "place_wall_mouse_no_cheese" >:: test_place_wall_mouse_no_cheese;
         "place_wall_mouse_moves_to_blank"
         >:: test_place_wall_mouse_moves_to_blank;
         "place_wall_mouse_blocked_path" >:: test_place_wall_mouse_blocked_path;
         "remove_wall_mouse_moves" >:: test_remove_wall_mouse_moves;
         "parse_coordinate_extra_errors" >:: test_parse_coordinate_extra_errors;
         "parse_error_to_string" >:: test_parse_error_to_string;
         "load_board_failure_cases" >:: test_load_board_failure_cases;
         "load_board_tip_marker_without_text"
         >:: test_load_board_tip_marker_without_text;
         "load_board_lowercase_tip" >:: test_load_board_lowercase_tip;
         "render_string_exact" >:: test_render_string_exact;
         "render_board_open_and_enclosed"
         >:: test_render_board_open_and_enclosed;
         "print_place_result" >:: test_print_place_result;
         "render_gui_size_helpers" >:: test_render_gui_size_helpers;
         "load_board_with_every_tile" >:: test_load_board_with_every_tile;
         "load_board_ignores_blank_grid_lines"
         >:: test_load_board_ignores_blank_grid_lines;
         "place_wall_mouse_moves_next_to_cheese_no_reset"
         >:: test_place_wall_mouse_moves_next_to_cheese_no_reset;
         "remove_wall_mouse_moves_next_to_cheese_no_reset"
         >:: test_remove_wall_mouse_moves_next_to_cheese_no_reset;
         "reachable_portal_same_id_already_visited"
         >:: test_reachable_portal_same_id_already_visited;
         "render_board_many_tiles_open" >:: test_render_board_many_tiles_open;
         "render_board_many_tiles_enclosed"
         >:: test_render_board_many_tiles_enclosed;
         "init_lava_bucket_out_of_bounds"
         >:: test_init_lava_bucket_out_of_bounds;
         "init_width_bug_case" >:: test_init_width_bug_case;
         "check_coord_placement_more_tiles"
         >:: test_check_coord_placement_more_tiles;
         "neighbors4_all_corners" >:: test_neighbors4_all_corners;
         "next_mouse_step_start_adjacent_returns_none"
         >:: test_next_mouse_step_start_adjacent_returns_none;
         "next_mouse_step_avoids_nonwalkable_tiles"
         >:: test_next_mouse_step_avoids_nonwalkable_tiles;
         "place_wall_on_special_tiles_fails"
         >:: test_place_wall_on_special_tiles_fails;
         "parser_file_not_found" >:: test_parser_file_not_found;
         "parse_coordinate_short_bad_formats"
         >:: test_parse_coordinate_short_bad_formats;
         "render_unload_gui_textures_none"
         >:: test_render_unload_gui_textures_none;
       ]
