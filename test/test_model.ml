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

(* Helper to create a standard test board Camel is at (2,2), Water is at (1,1),
   and we manually add a Wall at (3,3) *)
let setup_board () =
  let b =
    init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 }
      ~water:[ { r = 1; c = 1 } ]
      ~walls_available:10 ~max_score:0
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
         ( "camel_fully_enclosed" >:: fun _ ->
           let b =
             init ~width:5 ~height:5 ~camel:{ r = 2; c = 2 }
               ~water:[ { r = 0; c = 0 } ]
               ~walls_available:10 ~max_score:0
           in
           (* Box the camel completely in walls *)
           set_tile b { r = 1; c = 2 } Wall;
           set_tile b { r = 3; c = 2 } Wall;
           set_tile b { r = 2; c = 1 } Wall;
           set_tile b { r = 2; c = 3 } Wall;
           let result = reachable_from_camel b { r = 2; c = 2 } in
           (* The camel can only reach its own tile *)
           assert_coords_equal
             [ { r = 2; c = 2 } ]
             (coords_of_bool_array result.tiles);
           (* The score represents the area of the enclosure: just the camel
              tile (1) *)
           assert_equal ~printer:string_of_int 1 result.score );
         ( "camel_free_to_reach_everything" >:: fun _ ->
           let b =
             init ~width:3 ~height:3 ~camel:{ r = 1; c = 1 }
               ~water:[ { r = 2; c = 2 } ]
               ~walls_available:10 ~max_score:0
           in
           let result = reachable_from_camel b { r = 1; c = 1 } in
           (* The 3x3 board has 9 tiles total. 1 Camel + 1 Water = 2. Blank
              tiles = 7. The camel can reach all blank tiles + its own tile. It
              CANNOT traverse Water or Walls. *)
           assert_equal ~printer:string_of_int 8
             (List.length (coords_of_bool_array result.tiles));
           (* The complete board minus the water tile gives a score of 8 *)
           assert_equal ~printer:string_of_int 8 result.score );
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
               ~walls_available:0 ~max_score:0
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
       ]

let all_tests =
  "all model tests"
  >::: [ tests; neighbors4_tests; reachable_tests; place_wall_tests ]

let _ = run_test_tt_main all_tests
