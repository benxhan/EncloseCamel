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

(*Type to represent a pair of coordinates to placa a rock
Out_of_bounds has the coordinate (row, col)
Occupied has the type of tile currently ocupying the tile
*)
type place_result = 
| Out_of_bounds of int*int
| Occupied of tile
| Ok

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

(** [is_water board coord] returns [true] when tile is [water_ch]. *)
val is_water : board -> coordinate -> bool

(** [is_wall board coord] returns [true] when tile is [wall_ch]. *)
val is_wall : board -> coordinate -> bool

(** [is_blocked board coord] returns [true] for water or wall tiles.

  Expected behavior:
  - Treat only [water_ch] and [wall_ch] as blocked terrain.
  - Callers should still perform [in_bounds] checks where needed.
*)
val is_blocked : board -> coordinate -> bool

(** [is_free board coord] returns [true] for traversable non-camel tiles.

  Expected behavior:
  - [true] only when in bounds and tile is [empty_ch].
*)
val is_free : board -> coordinate -> bool

(** [place_wall board coord] attempts to place a wall on [coord].

    Expected behavior:
  - Reject placement on out-of-bounds/camel/water/existing-wall tiles.
  - Reject placement when [walls_remaining = 0].
  - On success, write [wall_ch] to the grid and decrement wall budget.
  - Return a new board state (functional update semantics).
*)
val place_wall : board -> coordinate -> board

(** [neighbors4 board coord] returns orthogonal in-bounds neighbors.

  Expected behavior:
  - Only up, down, left, right; no diagonals.
  - Exclude out-of-bounds coordinates.
*)
val neighbors4 : board -> coordinate -> coordinate list

(** [camel_moves board] returns legal one-step destinations for camel. *)
val camel_moves : board -> coordinate list

(** [move_camel board dest] moves camel one step if legal.

  Expected behavior:
  - Destination must be adjacent and traversable.
  - On success, old camel tile becomes [empty_ch] and destination becomes
    [camel_ch].
*)
val move_camel : board -> coordinate -> board

(** [reachable_from_camel board] returns unique tiles reachable from camel.

  Expected behavior:
  - Traverse by 4-direction movement through free tiles.
  - Include the camel tile in the output.
*)
val reachable_from_camel : board -> coordinate list

(** [reachable_area_size board] equals number of reachable tiles. *)
val reachable_area_size : board -> int

(** [camel_can_escape board] is [true] if camel can reach any boundary tile. *)
val camel_can_escape : board -> bool

(** [is_trapped board] indicates whether camel cannot escape. *)
val is_trapped : board -> bool

(** [score board] computes points as reachable free-to-roam area size. *)
val score : board -> int
