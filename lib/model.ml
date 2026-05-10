type coordinate = {
  r : int;
  c : int;
}

type tile =
  | Camel
  | Water
  | Wall
  | Blank
  | Cherry
  | Bees
  | GoldenApple
  | Portal of int
  | LavaBucket

let base_tiles =
  [ Camel; Water; Wall; Blank; Cherry; Bees; GoldenApple; LavaBucket ]

type tile_properties = {
  points : int;
  walkable : bool;
  file_char : char;
}

let properties_of = function
  | Blank -> { points = 1; walkable = true; file_char = 'G' }
  | Cherry -> { points = 5; walkable = true; file_char = 'R' }
  | Bees -> { points = -5; walkable = true; file_char = 'E' }
  | GoldenApple -> { points = 10; walkable = true; file_char = 'A' }
  | Portal _ -> { points = 1; walkable = true; file_char = 'P' }
  | LavaBucket -> { points = 1; walkable = true; file_char = 'L' }
  | Camel -> { points = 1; walkable = true; file_char = 'C' }
  | Water -> { points = 0; walkable = false; file_char = 'W' }
  | Wall -> { points = 0; walkable = false; file_char = 'B' }

type board = {
  grid : tile array array;
  walls_remaining : int;
  max_score : int;
  camel_loc : coordinate;
  tip : string option;
}

type place_result =
  | Out_of_bounds
  | Occupied
  | Ok

type enclosed_state =
  | Open
  | Enclosed of {
      tiles : bool array array;
      score : int;
      bonus_walls : int;
    }

let init ~width ~height ~camel ~water ~lava_buckets ~walls_available ~max_score
    =
  (* Dimension guard *)
  if width <= 0 || height <= 0 then
    failwith "init: width and height must be positive";

  (* Helper to validate that a coordinate is within the requested dimensions*)
  let in_bounds_dims { r; c } = r >= 0 && r < height && c >= 0 && c < height in

  (* Validate camel *)
  if not (in_bounds_dims camel) then
    failwith "init: camel coordinate is out of bounds";

  (* Validate waters to all be in bounds and have no repeats*)
  let val_list_inbounds =
    List.iter (fun w ->
        if not (in_bounds_dims w) then
          failwith "init: water coordiante is out of bounds")
  in

  val_list_inbounds water;
  let rec has_duplicate = function
    | [] -> false
    | h :: t -> List.mem h t || has_duplicate t
  in
  if has_duplicate water then failwith "init: duplicate water coordinates";

  (* Validate lava_buckets to all be in bounds and have no repeats*)
  val_list_inbounds lava_buckets;
  if has_duplicate lava_buckets then failwith "init: duplicate log coordinates";

  (* Camel must not share a tile with water *)
  if List.mem camel water then
    failwith "init: camel cannot be placed on a water tile";

  (* Build the grid *)
  let grid = Array.init height (fun _ -> Array.make width Blank) in
  List.iter (fun { r; c } -> grid.(r).(c) <- Water) water;
  List.iter (fun { r; c } -> grid.(r).(c) <- LavaBucket) lava_buckets;
  grid.(camel.r).(camel.c) <- Camel;

  { grid; walls_remaining = walls_available; max_score; camel_loc = camel; tip = None }

(* [in_bounds board coord] returns [true] if [coord] is on the board.

   Expected behavior: - Coordinates are zero-based. - A coordinate is in bounds
   when [0 <= x < board.width] and [0 <= y < board.height]. - The function is
   pure and must not modify [board]. *)

let get_tile _board _coord = _board.grid.(_coord.r).(_coord.c)
let set_tile _board _coord _tile = _board.grid.(_coord.r).(_coord.c) <- _tile

let check_coord_placement_log _board _coord =
  let in_bounds (_board : board) (_coord : coordinate) =
    let grid = _board.grid in
    _coord.r < Array.length grid
    && _coord.c < Array.length grid.(0)
    && _coord.r >= 0 && _coord.c >= 0
  in
  if not (in_bounds _board _coord) then Out_of_bounds
  else
    let coord_tile = get_tile _board _coord in
    if coord_tile = Blank || coord_tile = LavaBucket then Ok else Occupied

let check_coord_placement _board _coord =
  let in_bounds (_board : board) (_coord : coordinate) =
    let grid = _board.grid in
    _coord.r < Array.length grid
    && _coord.c < Array.length grid.(0)
    && _coord.r >= 0 && _coord.c >= 0
  in
  if not (in_bounds _board _coord) then Out_of_bounds
  else
    let coord_tile = get_tile _board _coord in
    if coord_tile = Blank then Ok else Occupied

let neighbors4 _board _coord =
  let grid = _board.grid in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let in_bounds r c = r >= 0 && r < height && c >= 0 && c < width in
  let acc = ref [] in
  if in_bounds (_coord.r + 1) _coord.c then
    acc := { r = _coord.r + 1; c = _coord.c } :: !acc;
  if in_bounds _coord.r (_coord.c + 1) then
    acc := { r = _coord.r; c = _coord.c + 1 } :: !acc;
  if in_bounds (_coord.r - 1) _coord.c then
    acc := { r = _coord.r - 1; c = _coord.c } :: !acc;
  if in_bounds _coord.r (_coord.c - 1) then
    acc := { r = _coord.r; c = _coord.c - 1 } :: !acc;
  !acc

let reachable_from_camel _board =
  let _grid = _board.grid in
  let height = Array.length _grid in
  let width = Array.length _grid.(0) in
  (* Initialize visited array to track seen tiles *)
  let visited = Array.init height (fun _ -> Array.make width false) in
  visited.(_board.camel_loc.r).(_board.camel_loc.c) <- true;
  let start_tile = _grid.(_board.camel_loc.r).(_board.camel_loc.c) in
  let _score = ref (properties_of start_tile).points in

  let enclosed_bonus_walls = ref 0 in
  let touches_edge = ref false in
  let is_on_edge r c = r = 0 || r = height - 1 || c = 0 || c = width - 1 in
  let rec dfs _coord =
    if not !touches_edge then
      begin if is_on_edge _coord.r _coord.c then touches_edge := true
      else
        let neighbors = neighbors4 _board _coord in
        List.iter
          (fun (crd : coordinate) ->
            if (not !touches_edge) && not visited.(crd.r).(crd.c) then
              let tile = _board.grid.(crd.r).(crd.c) in
              let props = properties_of tile in
              if props.walkable then begin
                visited.(crd.r).(crd.c) <- true;
                _score := !_score + props.points;

                (* specific hooks per tile type *)
                (match tile with
                | LavaBucket ->
                    enclosed_bonus_walls := !enclosed_bonus_walls + 3
                | Portal id ->
                    for r_idx = 0 to height - 1 do
                      for c_idx = 0 to width - 1 do
                        if (not !touches_edge) && not visited.(r_idx).(c_idx)
                        then
                          match _board.grid.(r_idx).(c_idx) with
                          | Portal match_id when match_id = id ->
                              visited.(r_idx).(c_idx) <- true;
                              let p_props = properties_of (Portal match_id) in
                              _score := !_score + p_props.points;
                              dfs { r = r_idx; c = c_idx }
                          | _ -> ()
                      done
                    done
                | _ -> ());

                dfs crd
              end)
          neighbors
      end
  in

  dfs _board.camel_loc;
  if !touches_edge then Open
  else
    Enclosed
      { tiles = visited; score = !_score; bonus_walls = !enclosed_bonus_walls }

let current_bonus_walls board =
  match reachable_from_camel board with
  | Open -> 0
  | Enclosed state -> state.bonus_walls

let place_wall board coord =
  match check_coord_placement board coord with
  | Out_of_bounds -> Error Out_of_bounds
  | Occupied ->
      (* if it's already a wall, remove it and adjust budget *)
      if get_tile board coord = Wall then (
        let old_bonus = current_bonus_walls board in
        set_tile board coord Blank;
        let new_bonus = current_bonus_walls board in
        let new_board =
          {
            board with
            walls_remaining = board.walls_remaining + 1 - old_bonus + new_bonus;
          }
        in
        Ok new_board)
      else Error Occupied
  | Ok ->
      if board.walls_remaining <= 0 then Error Occupied
      else
        let old_bonus = current_bonus_walls board in
        set_tile board coord Wall;
        let new_bonus = current_bonus_walls board in
        let new_board =
          {
            board with
            walls_remaining = board.walls_remaining - 1 - old_bonus + new_bonus;
          }
        in
        Ok new_board
