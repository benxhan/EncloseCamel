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

let init ~width:_ ~height:_ = failwith "TODO: Model.init"

let in_bounds _board _coord = failwith "TODO: Model.in_bounds"

let has_rock _board _coord = failwith "TODO: Model.has_rock"

let place_rock _board _coord = failwith "TODO: Model.place_rock"