type coordinate = { x : int; y : int }
type tile = 
  | Camel
  | Water
  | Wall 
  | Blank

type board = {
  grid : tile array array;
  walls_remaining : int;
}

(*Represents whether a wall placement was successful or if it failed, and why.
*)
type place_result = 
| Out_of_bounds
| Occupied
| Ok

type enclosed_state = {
  tiles : coordinate list;
  score : int;
}

(** [init ~width ~height ~camel ~water ~walls_available] constructs an initial
    board with a 2D character grid.

    Expected behavior:
    - The grid dimensions must match [height] rows and [width] columns.
    - All cells start as [empty_ch], then water and camel are placed.
    - [camel] must be in bounds and not on blocked terrain.
    - All water coordinates must be in bounds and unique.
    - [walls_remaining] starts at [walls_available].
    - Return [Error ...] for invalid initialization inputs.
*)
val init :
  width:int ->
  height:int ->
  camel:coordinate ->
  water:coordinate list ->
  walls_available:int ->
  board

(** [in_bounds board coord] returns [true] if [coord] is on the board.

    Expected behavior:
    - Coordinates are zero-based.
    - A coordinate is in bounds when [0 <= x < board.width] and
      [0 <= y < board.height].
    - The function is pure and must not modify [board].
*)
val in_bounds : board -> coordinate -> bool

(** [get_tile board coord] reads the grid character at [coord].

    Expected behavior:
  - Intended for in-bounds coordinates.
  - Should return one of the known tile characters.
*)
val get_tile : board -> coordinate -> char

(** [is_free board coord] returns [true] for traversable non-camel tiles.

  Expected behavior:
  - [true] only when in bounds and tile is [empty_ch].
*)
val is_free : board -> coordinate -> bool

(** [place_wall board coord] attempts to place a wall on [coord].

    Expected behavior:
  - Reject placement on out-of-bounds/camel/water/
  - If placement is on existing wall tile, remove it.
  - Reject placement when [walls_remaining = 0].
  - On success, write [wall_ch] to the grid and decrement wall budget.
  - Return a new board state (functional update semantics).
*)
val place_wall : board -> coordinate -> (board, place_result) result

(** [neighbors4 board coord] returns orthogonal in-bounds neighbors.

  Expected behavior:
  - Only up, down, left, right; no diagonals.
  - Exclude out-of-bounds coordinates.
*)
val neighbors4 : board -> coordinate -> coordinate list

(** [reachable_from_camel board] returns unique tiles reachable from camel.

  Expected behavior:
  - Traverse by 4-direction movement through free tiles.
  - Include the camel tile in the output.
*)
val reachable_from_camel : board -> enclosed_state