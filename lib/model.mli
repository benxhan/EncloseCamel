(** A position on the board grid. Coordinates are zero-based, with the origin
    [(0, 0)] at the top-left corner. [r] increases downward and [c] increases
    rightward. *)
type coordinate = {
  r : int;
  c : int;
}

(** The content of a single cell on the board.
    - [Blank] — an empty, traversable cell.
    - [Water] — a water cell; the camel must be enclosed away from it.
    - [Wall] — a player-placed wall; blocks movement.
    - [Camel] — the cell occupied by the camel. *)
type tile =
  | Camel
  | Water
  | Wall
  | Blank

(** The full state of the game board.
    - [grid] — a row-major 2-D array of tiles, indexed as [grid.(r).(c)].
    - [walls_remaining] — the number of walls the player may still place. *)
type board = {
  grid : tile array array;
  walls_remaining : int;
}

(** The outcome of a [place_wall] attempt.
    - [Out_of_bounds] — the target coordinate lies outside the grid.
    - [Occupied] — the cell is already taken by a non-wall tile (camel or
      water), or no wall budget remains.
    - [Ok] — the wall was placed (or removed) successfully. *)
type place_result =
  | Out_of_bounds
  | Occupied
  | Ok

type enclosed_state = {
  tiles : bool array array;
  score : int;
}

(** The result of a reachability query originating from the camel's position.
    - [tiles] — every coordinate reachable from the camel via 4-directional
      movement through [Blank] cells, including the camel's own cell.
    - [score] — the number of enclosed [Camel] and [Blank] tiles (i.e., the
      length of the [tiles] list, completely omitting [Water] tiles). *)
val init :
  width:int ->
  height:int ->
  camel:coordinate ->
  water:coordinate list ->
  walls_available:int ->
  board

(** [init ~width ~height ~camel ~water ~walls_available] constructs an initial
    board with a 2D tile grid.

    Expected behavior:
    - The grid has [height] rows and [width] columns, all initialised to
      [Blank].
    - [camel] is stamped onto the grid at the given coordinate.
    - Each coordinate in [water] is stamped as a [Water] tile.
    - [camel] must be in bounds and must not coincide with any water coordinate.
    - All water coordinates must be in bounds and pairwise distinct.
    - [walls_remaining] is initialised to [walls_available].
    - Raises [Failure] for any invalid input. *)

(** [set_tile board coord tile] changes the cell at [coord] to [tile].

    Expected behavior:
    - [coord] must be in bounds; behaviour is unspecified otherwise.
    - Uses functional update semantics — the original [board] is not modified.
    - [walls_remaining] is carried over unchanged. *)
val set_tile : board -> coordinate -> tile -> unit

(** [get_tile board coord] reads the grid character at [coord].

    Expected behavior:
    - Intended for in-bounds coordinates.
    - Should return one of the known tile characters. *)
val get_tile : board -> coordinate -> tile

(** [check_coord_placement board coord] returns whether a wall can be placed at
    [coord].

    Expected behavior:
    - Return [Out_of_bounds] if [coord] is outside the grid.
    - Return [Occupied] if [coord] has camel, water, or wall.
    - Return [Ok] if [coord] is blank and in bounds. *)
val check_coord_placement : board -> coordinate -> place_result

(** [place_wall board coord] attempts to place a wall on [coord].

    Expected behavior:
    - Reject placement on out-of-bounds/camel/water/
    - If placement is on existing wall tile, remove it.
    - Reject placement when [walls_remaining = 0].
    - On success, write [wall_ch] to the grid and decrement wall budget.
    - Return a new board state (functional update semantics). *)
val place_wall : board -> coordinate -> (board, place_result) result

(** [neighbors4 board coord] returns orthogonal in-bounds neighbors.

    Expected behavior:
    - Only up, down, left, right; no diagonals.
    - Exclude out-of-bounds coordinates. *)
val neighbors4 : board -> coordinate -> coordinate list

(** [reachable_from_camel board] returns unique tiles reachable from camel.

    Expected behavior:
    - Traverse by 4-direction movement through free tiles.
    - Include the camel tile in the output. Requires:
    - [coordinate] is the location of camel. *)
val reachable_from_camel : board -> coordinate -> enclosed_state
