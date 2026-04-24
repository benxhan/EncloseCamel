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

type enclosed_state = {
  tiles : coordinate list;
  score : int;
}

let init ~width:_ ~height:_ ~camel:_ ~water:_ ~walls_available:_ =
	failwith "TODO: Model.init"

let in_bounds (_board : board) (_coord : coordinate) = 
  let grid = _board.grid in
  _coord.x < Array.length grid && 
  _coord.y < Array.length grid.(0) &&
  _coord.x >= 0 && _coord.y >= 0

let get_tile _board _coord = failwith "TODO: Model.get_tile"

let is_free _board _coord = 
  in_bounds _board _coord 
  && let coord_tile = get_tile _board _coord in 
    (coord_tile <> Water && coord_tile <> Wall)

let place_wall _board _coord = failwith "TODO: Model.place_wall"

let neighbors4 _board _coord = failwith "TODO: Model.neighbors4"

let reachable_from_camel _board = failwith "TODO: Model.reachable_from_camel"