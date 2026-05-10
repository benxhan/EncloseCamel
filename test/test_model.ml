open EncloseCamel
open Model
open OUnit2

let string_of_place_result = function
  | Out_of_bounds -> "Out_of_bounds"
  | Occupied -> "Occupied"
  | Ok -> "Ok"

let string_of_tile = function
  | Camel -> "Camel"
  | Water -> "Water"
  | Wall -> "Wall"
  | Blank -> "Blank"
  | Cherry -> "Cherry"
  | Bees -> "Bees"
  | GoldenApple -> "GoldenApple"
  | Portal id -> "Portal " ^ string_of_int id
  | LavaBucket -> "LavaBucket"
  | Mouse -> "Mouse"
  | Cheese -> "Cheese"

(* Helper to create a standard test board Camel is at (2,2), Water is at (1,1),
   and we manually add a Wall at (3,3) *)
let setup_board () =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 }
      ~water:[ { r = 1; c = 1 } ]
      ~lava_buckets:[] ~walls_available:10 ~max_score:0
  in
  set_tile b { r = 3; c = 3 } Wall;
  b

let tests =
  "check_coord_placement tests"
  >::: [
         ( "out_of_bounds_negative" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Out_of_bounds
             (check_coord_placement b { r = -1; c = 0 }) );
         ( "out_of_bounds_too_large" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Out_of_bounds
             (check_coord_placement b { r = 6; c = 2 }) );
         ( "occupied_by_camel" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Occupied
             (check_coord_placement b { r = 2; c = 2 }) );
         ( "occupied_by_water" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Occupied
             (check_coord_placement b { r = 1; c = 1 }) );
         ( "occupied_by_wall" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Occupied
             (check_coord_placement b { r = 3; c = 3 }) );
         ( "ok_blank_tile" >:: fun _ ->
           let b = setup_board () in
           assert_equal ~printer:string_of_place_result Ok
             (check_coord_placement b { r = 0; c = 0 }) );
       ]

let sort_coords lst =
  List.sort
    (fun a b -> if a.r = b.r then compare a.c b.c else compare a.r b.r)
    lst

let assert_coords_equal expected actual =
  assert_equal
    ~printer:(fun lst ->
      "["
      ^ String.concat "; "
          (List.map
             (fun coord -> Printf.sprintf "{r=%d; c=%d}" coord.r coord.c)
             lst)
      ^ "]")
    (sort_coords expected) (sort_coords actual)

let neighbors4_tests =
  "neighbors4 tests"
  >::: [
         ( "center_tile_4_neighbors" >:: fun _ ->
           let b = setup_board () in
           assert_coords_equal
             [
               { r = 1; c = 2 };
               { r = 3; c = 2 };
               { r = 2; c = 1 };
               { r = 2; c = 3 };
             ]
             (neighbors4 b { r = 2; c = 2 }) );
         ( "top_left_corner_2_neighbors" >:: fun _ ->
           let b = setup_board () in
           assert_coords_equal
             [ { r = 0; c = 1 }; { r = 1; c = 0 } ]
             (neighbors4 b { r = 0; c = 0 }) );
         ( "bottom_edge_3_neighbors" >:: fun _ ->
           let b = setup_board () in
           assert_coords_equal
             [ { r = 4; c = 1 }; { r = 4; c = 3 }; { r = 3; c = 2 } ]
             (neighbors4 b { r = 4; c = 2 }) );
       ]

let coords_of_bool_array arr =
  let coords = ref [] in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c is_reachable ->
          if is_reachable then coords := { r; c } :: !coords)
        row)
    arr;
  !coords

let reachable_tests =
  "reachable_from_camel tests"
  >::: [
         ( "special_tiles_score_in_enclosure" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 1; c = 1 } ~water:[]
               ~lava_buckets:[] ~walls_available:20 ~max_score:0
           in
           let walls =
             [
               { r = 0; c = 1 };
               { r = 0; c = 2 };
               { r = 3; c = 1 };
               { r = 3; c = 2 };
               { r = 1; c = 0 };
               { r = 2; c = 0 };
               { r = 1; c = 3 };
               { r = 2; c = 3 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) walls;
           set_tile b { r = 1; c = 2 } Cherry;
           set_tile b { r = 2; c = 1 } Bees;
           set_tile b { r = 2; c = 2 } GoldenApple;

           let result = reachable_from_camel b in
           match result with
           | Enclosed { score; _ } ->
               assert_equal ~printer:string_of_int 11 score
                 ~msg:
                   "Score should be 1(init) + 5(Cherry) - 5(Bees) + 10(Apple) \
                    = 11"
           | Open -> assert_failure "Expected Enclosed, but got Open" );
         ( "lava_buckets_bonus_walls" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 1; c = 1 } ~water:[]
               ~lava_buckets:[] ~walls_available:20 ~max_score:0
           in
           let walls =
             [
               { r = 0; c = 1 };
               { r = 0; c = 2 };
               { r = 3; c = 1 };
               { r = 3; c = 2 };
               { r = 1; c = 0 };
               { r = 2; c = 0 };
               { r = 1; c = 3 };
               { r = 2; c = 3 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) walls;
           set_tile b { r = 1; c = 2 } LavaBucket;
           set_tile b { r = 2; c = 1 } LavaBucket;

           let result = reachable_from_camel b in
           match result with
           | Enclosed { bonus_walls; _ } ->
               assert_equal ~printer:string_of_int 6 bonus_walls
                 ~msg:"Each LavaBucket gives 3 bonus walls"
           | Open -> assert_failure "Expected Enclosed, but got Open" );
         ( "camel_fully_enclosed" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 }
               ~water:[ { r = 0; c = 0 } ]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           (* Box the camel completely in walls *)
           set_tile b { r = 1; c = 2 } Wall;
           set_tile b { r = 3; c = 2 } Wall;
           set_tile b { r = 2; c = 1 } Wall;
           set_tile b { r = 2; c = 3 } Wall;
           let result = reachable_from_camel b in
           match result with
           | Enclosed { tiles; score; bonus_walls = _ } ->
               (* The camel can only reach its own tile *)
               assert_coords_equal
                 [ { r = 2; c = 2 } ]
                 (coords_of_bool_array tiles);
               (* The score represents the area of the enclosure: just the camel
                  tile (1) *)
               assert_equal ~printer:string_of_int 1 score
           | Open -> assert_failure "Expected Enclosed, but got Open" );
         ( "camel_free_to_reach_everything" >:: fun _ ->
           let b =
             init ~width:3 ~height:3 ~camel:{ r = 1; c = 1 }
               ~water:[ { r = 2; c = 2 } ]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           let result = reachable_from_camel b in
           match result with
           | Open -> ()
           | Enclosed _ ->
               assert_failure
                 "Expected Open because camel can reach the edge bounding the \
                  board" );
       ]

let place_wall_tests =
  "place_wall tests"
  >::: [
         ( "place_out_of_bounds" >:: fun _ ->
           let b = setup_board () in
           match place_wall b { r = -1; c = 0 } with
           | Error Out_of_bounds -> ()
           | _ -> assert_failure "Expected Error Out_of_bounds" );
         ( "place_on_camel_fails" >:: fun _ ->
           let b = setup_board () in
           match place_wall b { r = 2; c = 2 } with
           | Error Occupied -> ()
           | _ -> assert_failure "Expected Error Occupied" );
         ( "place_on_water_fails" >:: fun _ ->
           let b = setup_board () in
           match place_wall b { r = 1; c = 1 } with
           | Error Occupied -> ()
           | _ -> assert_failure "Expected Error Occupied" );
         ( "place_no_walls_budget" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:0 ~max_score:0
           in
           match place_wall b { r = 0; c = 0 } with
           | Error Occupied -> ()
           | _ -> assert_failure "Expected Error Occupied when out of budget" );
         ( "place_on_blank_success" >:: fun _ ->
           let b = setup_board () in
           let r = 0 in
           let c = 0 in
           let og_walls = b.walls_remaining in
           match place_wall b { r; c } with
           | Ok new_board ->
               assert_equal Wall
                 (get_tile new_board { r; c })
                 ~printer:string_of_tile ~msg:"Tile should be Wall";
               assert_equal (og_walls - 1) new_board.walls_remaining
                 ~printer:string_of_int ~msg:"Walls should decrement"
           | _ -> assert_failure "Expected Ok" );
         ( "place_remove_wall_success" >:: fun _ ->
           let b = setup_board () in
           let r = 3 in
           let c = 3 in
           (* already a Wall in setup_board *)
           let og_walls = b.walls_remaining in
           match place_wall b { r; c } with
           | Ok new_board ->
               assert_equal Blank
                 (get_tile new_board { r; c })
                 ~printer:string_of_tile ~msg:"Tile should return to Blank";
               assert_equal (og_walls + 1) new_board.walls_remaining
                 ~printer:string_of_int ~msg:"Walls should increment"
           | _ -> assert_failure "Expected Ok" );
         ( "dynamic_lavabucket_bonus" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:6 ~max_score:0
           in
           let walls =
             [
               { r = 1; c = 2 };
               { r = 3; c = 2 };
               { r = 2; c = 1 };
               { r = 1; c = 3 };
               { r = 2; c = 4 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) walls;
           (* remaining is implicitly 6 - 5 = 1, but we didn't call place_wall,
              we bypassed it. Let's manually update walls_remaining to 1 so the
              test resembles valid state *)
           let b = { b with walls_remaining = 1 } in
           set_tile b { r = 2; c = 3 } LavaBucket;
           (* Camel is currently open. Place last wall to enclose it. *)
           let b1 =
             match place_wall b { r = 3; c = 3 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place"
           in
           (* Enclosing should give +3 bonus walls. So we had 1 base, used 1.
              Base is 0 now. Bonus is 3. Total is 3. *)
           assert_equal 3 b1.walls_remaining ~printer:string_of_int
             ~msg:"Should get 3 bonus walls";
           (* We place another wall, reducing remaining to 2 *)
           let b2 =
             match place_wall b1 { r = 0; c = 0 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place bonus"
           in
           assert_equal 2 b2.walls_remaining ~printer:string_of_int
             ~msg:"Used a bonus wall";
           (* Now we remove a wall from the enclosure, breaking it. *)
           let b3 =
             match place_wall b2 { r = 1; c = 2 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should remove"
           in
           (* We get 1 back for the removed wall, but lose the 3 bonus walls. 2
              + 1 - 3 = 0 walls remaining. *)
           assert_equal 0 b3.walls_remaining ~printer:string_of_int
             ~msg:"Lost bonus walls";
           (* Try negative. Let's place it back to enclose? But wait, budget is
              0! So it should FAIL! *)
           begin match place_wall b3 { r = 1; c = 2 } with
           | Error Occupied -> ()
           | _ -> assert_failure "Should NOT be able to place with 0 budget"
           end;
           (* Remove another wall to get back to positive budget! *)
           let b4 =
             match place_wall b3 { r = 2; c = 1 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should remove again"
           in
           assert_equal 1 b4.walls_remaining ~printer:string_of_int
             ~msg:"Got 1 positive budget";
           (* Now place wall at {r=1; c=2} again to see if it encloses? Wait, if
              we removed (2,1), it won't enclose! Let's re-place at (2,1) so we
              are back to 0 budget. Then remove (0,0) (which is a bonus
              non-essential wall) *)
           let b5 =
             match place_wall b4 { r = 2; c = 1 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place"
           in
           let b6 =
             match place_wall b5 { r = 0; c = 0 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should remove"
           in
           assert_equal 1 b6.walls_remaining ~printer:string_of_int
             ~msg:"Budget is 1";
           (* Place at (1,2) to enclose again *)
           let b7 =
             match place_wall b6 { r = 1; c = 2 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place and enclose"
           in
           assert_equal 3 b7.walls_remaining ~printer:string_of_int
             ~msg:"Got bonus walls again" );
         ( "multiple_lava_buckets_in_same_enclosure" >:: fun _ ->
           let b =
             init ~width:6 ~height:6 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           let walls =
             [
               { r = 1; c = 1 };
               { r = 1; c = 2 };
               { r = 1; c = 3 };
               { r = 2; c = 1 };
               { r = 2; c = 4 };
               { r = 3; c = 1 };
               { r = 3; c = 2 };
               { r = 3; c = 3 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) walls;
           let b = { b with walls_remaining = 1 } in
           set_tile b { r = 2; c = 4 } Blank;
           set_tile b { r = 2; c = 3 } LavaBucket;
           set_tile b { r = 4; c = 4 } LavaBucket;

           let b1 =
             match place_wall b { r = 2; c = 4 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place wall"
           in
           assert_equal 3 b1.walls_remaining ~printer:string_of_int
             ~msg:"Should get exactly 3 bonus for the inside bucket" );
         ( "double_lava_buckets_enclosed" >:: fun _ ->
           let b =
             init ~width:6 ~height:6 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           let walls =
             [
               { r = 1; c = 1 };
               { r = 1; c = 2 };
               { r = 1; c = 3 };
               { r = 1; c = 4 };
               { r = 2; c = 1 };
               { r = 2; c = 5 };
               { r = 3; c = 1 };
               { r = 3; c = 2 };
               { r = 3; c = 3 };
               { r = 3; c = 4 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) walls;
           let b = { b with walls_remaining = 1 } in
           set_tile b { r = 2; c = 5 } Blank;
           set_tile b { r = 2; c = 3 } LavaBucket;
           set_tile b { r = 2; c = 4 } LavaBucket;

           let b1 =
             match place_wall b { r = 2; c = 5 } with
             | Ok nb -> nb
             | _ -> assert_failure "Should place wall"
           in
           assert_equal 6 b1.walls_remaining ~printer:string_of_int
             ~msg:"Should get exactly 6 bonus for 2 inside buckets" );
       ]

let setup_board_with_lava_buckets () =
  init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 }
    ~water:[ { r = 1; c = 1 } ]
    ~lava_buckets:[ { r = 0; c = 1 }; { r = 4; c = 4 } ]
    ~walls_available:10 ~max_score:0

let lava_buckets_tests =
  "logs tests"
  >::: [
         ( "lava_buckets_are_initialized_on_grid" >:: fun _ ->
           let b = setup_board_with_lava_buckets () in
           assert_equal LavaBucket
             (get_tile b { r = 0; c = 1 })
             ~printer:string_of_tile;
           assert_equal LavaBucket
             (get_tile b { r = 4; c = 4 })
             ~printer:string_of_tile );
         ( "lava_buckets_count_as_occupied" >:: fun _ ->
           let b = setup_board_with_lava_buckets () in
           assert_equal ~printer:string_of_place_result Occupied
             (check_coord_placement b { r = 0; c = 1 }) );
         ( "cannot_place_wall_on_lava_buckets" >:: fun _ ->
           let b = setup_board_with_lava_buckets () in
           match place_wall b { r = 0; c = 1 } with
           | Error Occupied -> ()
           | _ -> assert_failure "Expected Error Occupied on LavaBucket tile" );
       ]

let portal_tests =
  "portal tests"
  >::: [
         ( "portals_connect_enclosed_areas" >:: fun _ ->
           let b =
             init ~width:10 ~height:10 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:20 ~max_score:0
           in
           (* Box A: encloses (2,2) and (2,3) *)
           let box_a_walls =
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
           List.iter (fun w -> set_tile b w Wall) box_a_walls;
           set_tile b { r = 2; c = 3 } (Portal 0);

           (* Box B: encloses (6,6) and (6,7) *)
           let box_b_walls =
             [
               { r = 5; c = 5 };
               { r = 5; c = 6 };
               { r = 5; c = 7 };
               { r = 5; c = 8 };
               { r = 6; c = 5 };
               { r = 6; c = 8 };
               { r = 7; c = 5 };
               { r = 7; c = 6 };
               { r = 7; c = 7 };
               { r = 7; c = 8 };
             ]
           in
           List.iter (fun w -> set_tile b w Wall) box_b_walls;
           set_tile b { r = 6; c = 6 } (Portal 0);

           (* (6,7) is implicitly a Blank tile worth 1 point *)
           let result = reachable_from_camel b in
           match result with
           | Enclosed { score; _ } ->
               (* Calculation: Camel at (2,2) = 1 pt -> explicit 1 point just
                  like a Blank tile Portal 0 at (2,3) = 1 pt Portal 0 at (6,6) =
                  1 pt Blank at (6,7) = 1 pt. Total = 4 pts *)
               assert_equal ~printer:string_of_int 4 score
                 ~msg:"Score should combine Box A and Box B via portal jump"
           | Open -> assert_failure "Expected Enclosed, but got Open" );
         ( "portal_leaks_to_open_area" >:: fun _ ->
           let b =
             init ~width:10 ~height:10 ~camel:{ r = 2; c = 2 } ~water:[]
               ~lava_buckets:[] ~walls_available:20 ~max_score:0
           in
           (* Box A: safely encloses the camel... *)
           let box_a_walls =
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
           List.iter (fun w -> set_tile b w Wall) box_a_walls;
           set_tile b { r = 2; c = 3 } (Portal 0);

           (* But Portal 0 at (8,8) is unrestricted and will reach the map
              edge *)
           set_tile b { r = 8; c = 8 } (Portal 0);

           let result = reachable_from_camel b in
           match result with
           | Open ->
               ()
               (* Since the second portal reaches the edge, the whole DFS
                  touches_edge *)
           | Enclosed _ ->
               assert_failure
                 "Expected Open since second portal reaches the edge" );
       ]

let a_star_tests =
  "a star pathfinding tests"
  >::: [
         ( "direct_path" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           set_tile b { r = 0; c = 0 } Mouse;
           set_tile b { r = 0; c = 3 } Cheese;
           (* Expected to move right from (0,0) towards (0,3) -> next step is
              (0,1) *)
           assert_equal
             (Some { r = 0; c = 1 })
             (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 3 }) );
         ( "already_adjacent" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           set_tile b { r = 0; c = 0 } Mouse;
           set_tile b { r = 0; c = 1 } Cheese;
           assert_equal (Some { r = 0; c = 1 })
             (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 1 }) );
         ( "obstacle_avoidance" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           set_tile b { r = 0; c = 0 } Mouse;
           set_tile b { r = 0; c = 2 } Cheese;
           set_tile b { r = 0; c = 1 } Wall;
           (* Direct path right (0,1) is blocked by Wall. Must go down to
              (1,0) *)
           assert_equal
             (Some { r = 1; c = 0 })
             (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 2 }) );
         ( "unreachable" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           set_tile b { r = 0; c = 0 } Mouse;
           set_tile b { r = 0; c = 2 } Cheese;
           (* Box the mouse in completely *)
           set_tile b { r = 0; c = 1 } Wall;
           set_tile b { r = 1; c = 0 } Wall;
           set_tile b { r = 1; c = 1 } Wall;
           assert_equal None
             (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 2 }) );
         ( "portal_treated_as_wall" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 4; c = 4 } ~water:[]
               ~lava_buckets:[] ~walls_available:10 ~max_score:0
           in
           set_tile b { r = 0; c = 0 } Mouse;
           set_tile b { r = 0; c = 2 } Cheese;
           set_tile b { r = 0; c = 1 } (Portal 1);
           (* Direct path right (0,1) is blocked by Portal. Must go down to
              (1,0) *)
           assert_equal
             (Some { r = 1; c = 0 })
             (next_mouse_step b { r = 0; c = 0 } { r = 0; c = 2 }) );
       ]

let all_tests =
  "all model tests"
  >::: [
         tests;
         neighbors4_tests;
         reachable_tests;
         place_wall_tests;
         lava_buckets_tests;
         portal_tests;
         a_star_tests;
       ]

let _ = run_test_tt_main all_tests
