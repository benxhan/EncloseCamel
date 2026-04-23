type coordinate = {
  x : int;
  y : int;
}

type tile =
  | Camel
  | Water
  | Wall
  | Blank

type board = {
  grid : tile array array;
  walls_remaining : int;
}

<<<<<<< HEAD
type enclosed_state = {
  tiles : coordinate list;
  score : int;
}

=======
val init :
  width:int ->
  height:int ->
  camel:coordinate ->
  water:coordinate list ->
  walls_available:int ->
  board
>>>>>>> df3cb1b (parser.ml completed parse_coordinate and parse_error_to_string)
(** [init ~width ~height ~camel ~water ~walls_available] constructs an initial
    board with a 2D character grid.

    Expected behavior:
    - The grid dimensions must match [height] rows and [width] columns.
    - All cells start as [empty_ch], then water and camel are placed.
    - [camel] must be in bounds and not on blocked terrain.
    - All water coordinates must be in bounds and unique.
    - [walls_remaining] starts at [walls_available].
    - Return [Error ...] for invalid initialization inputs. *)

val in_bounds : board -> coordinate -> bool
(** [in_bounds board coord] returns [true] if [coord] is on the board.

    Expected behavior:
    - Coordinates are zero-based.
    - A coordinate is in bounds when [0 <= x < board.width] and
      [0 <= y < board.height].
    - The function is pure and must not modify [board]. *)

val get_tile : board -> coordinate -> char
(** [get_tile board coord] reads the grid character at [coord].

    Expected behavior:
    - Intended for in-bounds coordinates.
    - Should return one of the known tile characters. *)

<<<<<<< HEAD
=======
val is_water : board -> coordinate -> bool
(** [is_water board coord] returns [true] when tile is [water_ch]. *)

val is_wall : board -> coordinate -> bool
(** [is_wall board coord] returns [true] when tile is [wall_ch]. *)

val is_blocked : board -> coordinate -> bool
(** [is_blocked board coord] returns [true] for water or wall tiles.

    Expected behavior:
    - Treat only [water_ch] and [wall_ch] as blocked terrain.
    - Callers should still perform [in_bounds] checks where needed. *)

val is_free : board -> coordinate -> bool
>>>>>>> df3cb1b (parser.ml completed parse_coordinate and parse_error_to_string)
(** [is_free board coord] returns [true] for traversable non-camel tiles.

    Expected behavior:
    - [true] only when in bounds and tile is [empty_ch]. *)

val place_wall : board -> coordinate -> board
(** [place_wall board coord] attempts to place a wall on [coord].

    Expected behavior:
<<<<<<< HEAD
  - Reject placement on out-of-bounds/camel/water/
  - If placement is on existing wall tile, remove it.
  - Reject placement when [walls_remaining = 0].
  - On success, write [wall_ch] to the grid and decrement wall budget.
  - Return a new board state (functional update semantics).
*)
val place_wall : board -> coordinate -> board
=======
    - Reject placement on out-of-bounds/camel/water/existing-wall tiles.
    - Reject placement when [walls_remaining = 0].
    - On success, write [wall_ch] to the grid and decrement wall budget.
    - Return a new board state (functional update semantics). *)
>>>>>>> df3cb1b (parser.ml completed parse_coordinate and parse_error_to_string)

val neighbors4 : board -> coordinate -> coordinate list
(** [neighbors4 board coord] returns orthogonal in-bounds neighbors.

    Expected behavior:
    - Only up, down, left, right; no diagonals.
    - Exclude out-of-bounds coordinates. *)

<<<<<<< HEAD
(** [reachable_from_camel board] returns unique tiles reachable from camel.

  Expected behavior:
  - Traverse by 4-direction movement through free tiles.
  - Include the camel tile in the output.
*)
val reachable_from_camel : board -> enclosed_state
=======
val camel_moves : board -> coordinate list
(** [camel_moves board] returns legal one-step destinations for camel. *)

val move_camel : board -> coordinate -> board
(** [move_camel board dest] moves camel one step if legal.

    Expected behavior:
    - Destination must be adjacent and traversable.
    - On success, old camel tile becomes [empty_ch] and destination becomes
      [camel_ch]. *)

val reachable_from_camel : board -> coordinate list
(** [reachable_from_camel board] returns unique tiles reachable from camel.

    Expected behavior:
    - Traverse by 4-direction movement through free tiles.
    - Include the camel tile in the output. *)

val reachable_area_size : board -> int
(** [reachable_area_size board] equals number of reachable tiles. *)

val camel_can_escape : board -> bool
(** [camel_can_escape board] is [true] if camel can reach any boundary tile. *)

val is_trapped : board -> bool
(** [is_trapped board] indicates whether camel cannot escape. *)

val score : board -> int
(** [score board] computes points as reachable free-to-roam area size. *)
>>>>>>> df3cb1b (parser.ml completed parse_coordinate and parse_error_to_string)
