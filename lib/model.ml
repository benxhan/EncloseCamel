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

type place_result =
  | Out_of_bounds
  | Occupied
  | Ok

type enclosed_state = {
  tiles : coordinate list;
  score : int;
}

let init ~width:_ ~height:_ ~camel:_ ~water:_ ~walls_available:_ =
  failwith "TODO: Model.init"

(* [in_bounds board coord] returns [true] if [coord] is on the board.

   Expected behavior: - Coordinates are zero-based. - A coordinate is in bounds
   when [0 <= x < board.width] and [0 <= y < board.height]. - The function is
   pure and must not modify [board]. *)
let in_bounds (_board : board) (_coord : coordinate) =
  let grid = _board.grid in
  _coord.x < Array.length grid
  && _coord.y < Array.length grid.(0)
  && _coord.x >= 0 && _coord.y >= 0

let get_tile _board _coord = failwith "TODO: Model.get_tile"

let check_coord_placement _board _coord =
  if not (in_bounds _board _coord) then Out_of_bounds
  else
    let coord_tile = get_tile _board _coord in
    if coord_tile <> Water && coord_tile <> Wall then Occupied else Ok

let place_wall _board _coord = failwith "TODO: Model.place_wall"
let neighbors4 _board _coord = failwith "TODO: Model.neighbors4"
let reachable_from_camel _board = failwith "TODO: Model.reachable_from_camel"
