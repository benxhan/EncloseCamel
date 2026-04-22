type coordinate = { x : int; y : int }

type board = {
  width : int;
  height : int;
  rocks : coordinate list;
}

type place_result =
  | Ok of board
  | Out_of_bounds
  | Occupied

(** [init ~width ~height] creates a new empty board with the provided dimensions.

    Expected behavior:
    - Width and height represent the full valid coordinate range of the board.
    - The returned board starts with zero rocks.
    - The implementation should reject non-positive dimensions (for example by
      raising [Invalid_argument]) so callers cannot create impossible boards.
*)
val init : width:int -> height:int -> board

(** [in_bounds board coord] returns [true] if [coord] is on the board.

    Expected behavior:
    - Coordinates are zero-based.
    - A coordinate is in bounds when [0 <= x < board.width] and
      [0 <= y < board.height].
    - The function is pure and must not modify [board].
*)
val in_bounds : board -> coordinate -> bool

(** [has_rock board coord] reports whether [coord] is already occupied.

    Expected behavior:
    - Returns [true] exactly when [coord] appears in [board.rocks].
    - Equality should compare both [x] and [y].
    - The function is pure and must not modify [board].
*)
val has_rock : board -> coordinate -> bool

(** [place_rock board coord] attempts to place one rock at [coord].

    Expected behavior:
    - Return [Out_of_bounds] when [coord] is outside the board.
    - Return [Occupied] when a rock already exists at [coord].
    - Return [Ok next_board] when placement succeeds.
    - On success, [next_board] should contain all existing rocks plus [coord].
    - The original [board] should remain unchanged (functional update style).
*)
val place_rock : board -> coordinate -> place_result
