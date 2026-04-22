type tile = 
  | Camel
  | Water
  | Wall 
  | Blank

type board = {
	grid : char array array;
	walls_remaining : int;
}

type init_error =
	| Non_positive_dimensions
	| Negative_wall_budget
	| Camel_out_of_bounds
	| Camel_on_water
	| Invalid_water_coordinate
	| Duplicate_water_coordinate

type place_wall_error =
	| No_walls_left
	| Out_of_bounds
	| On_camel
	| On_water
	| Occupied_by_wall

type move_camel_error =
	| Move_out_of_bounds
	| Move_blocked
	| Not_adjacent

let empty_ch = '.'

let camel_ch = 'C'

let water_ch = '~'

let wall_ch = '#'

let init ~width:_ ~height:_ ~camel:_ ~water:_ ~walls_available:_ =
	failwith "TODO: Model.init"

let in_bounds _board _coord = failwith "TODO: Model.in_bounds"

let get_tile _board _coord = failwith "TODO: Model.get_tile"

let is_water _board _coord = failwith "TODO: Model.is_water"

let is_wall _board _coord = failwith "TODO: Model.is_wall"

let is_blocked _board _coord = failwith "TODO: Model.is_blocked"

let is_free _board _coord = failwith "TODO: Model.is_free"

let place_wall _board _coord = failwith "TODO: Model.place_wall"

let neighbors4 _board _coord = failwith "TODO: Model.neighbors4"

let camel_moves _board = failwith "TODO: Model.camel_moves"

let move_camel _board _coord = failwith "TODO: Model.move_camel"

let reachable_from_camel _board = failwith "TODO: Model.reachable_from_camel"

let reachable_area_size _board = failwith "TODO: Model.reachable_area_size"

let camel_can_escape _board = failwith "TODO: Model.camel_can_escape"

let is_trapped _board = failwith "TODO: Model.is_trapped"

let score _board = failwith "TODO: Model.score"