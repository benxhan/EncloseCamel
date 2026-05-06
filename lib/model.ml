type coordinate = {
  r : int;
  c : int;
}

type tile =
  | Camel
  | Water
  | Wall
  | Blank
  | Logs


type board = {
  grid : tile array array;
  walls_remaining : int;
  bonus_walls : int ref;
  max_score : int;
  camel_loc : coordinate;
  bonused_logs : coordinate list ref;
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

let init ~width ~height ~camel ~water ~logs ~walls_available ~max_score =
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

  (* Validate logs to all be in bounds and have no repeats*)
  val_list_inbounds logs;
  if has_duplicate logs then failwith "init: duplicate log coordinates";

  (* Camel must not share a tile with water *)
  if List.mem camel water then
    failwith "init: camel cannot be placed on a water tile";

  (* Build the grid *)
  let grid = Array.init height (fun _ -> Array.make width Blank) in
  List.iter (fun { r; c } -> grid.(r).(c) <- Water) water;
  List.iter (fun { r; c } -> grid.(r).(c) <- Logs) logs;
  grid.(camel.r).(camel.c) <- Camel;

  {
    grid;
    walls_remaining = walls_available;
    bonus_walls = ref 0;
    max_score;
    camel_loc = camel;
    bonused_logs = ref [];
  }

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
    if coord_tile = Blank || coord_tile = Logs then Ok else Occupied

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

let place_wall board coord =
  match check_coord_placement board coord with
  | Out_of_bounds -> Error Out_of_bounds
  | Occupied ->
      (* if it's already a wall, remove it and refund the budget *)
      if get_tile board coord = Wall then (
        let new_board =
          { board with walls_remaining = board.walls_remaining + 1 }
        in
        set_tile new_board coord Blank;
        Ok new_board)
      else Error Occupied
  | Ok ->
      (* if it's blank but we have no walls left, treat as occupied based on
         tests *)
      if board.walls_remaining + !(board.bonus_walls) <= 0 then Error Occupied
      else
        let new_board =
          if board.walls_remaining > 0 then
            { board with walls_remaining = board.walls_remaining - 1 }
          else { board with bonus_walls = ref (!(board.bonus_walls) - 1) }
        in
        set_tile new_board coord Wall;
        Ok new_board

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
  let _score = ref 1 in

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
              match _board.grid.(crd.r).(crd.c) with
              | Blank ->
                  visited.(crd.r).(crd.c) <- true;
                  _score := !_score + 1;
                  dfs crd
              | Logs ->
                  visited.(crd.r).(crd.c) <- true;
                  (* Only count bonus if this log hasn't been bonused yet *)
                  if not (List.mem crd !(_board.bonused_logs)) then
                    enclosed_bonus_walls := !enclosed_bonus_walls + 3
              | Camel ->
                  visited.(crd.r).(crd.c) <- true;
                  dfs crd
              | Water | Wall -> ())
          neighbors
      end
  in
  dfs _board.camel_loc;
  if !touches_edge then Open
  else
    Enclosed
      { tiles = visited; score = !_score; bonus_walls = !enclosed_bonus_walls }
