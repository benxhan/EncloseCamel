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

let init ~width ~height ~camel ~water ~walls_available =
  (* Dimension guard *)
  if width <= 0 || height <= 0 then
    failwith "init: width and height must be positive";

  (* Helper to validate that a coordinate is within the requested dimensions*)
  let in_bounds_dims { x; y } = x >= 0 && x < width && y >= 0 && y < height in

  (* Validate camel *)
  if not (in_bounds_dims camel) then
    failwith "init: camel coordinate is out of bounds";

  (* Validate waters to all be in bounds and have no repeats*)
  List.iter
    (fun w ->
      if not (in_bounds_dims w) then
        failwith "init: water coordiante is out of bounds")
    water;
  let rec has_duplicate = function
    | [] -> false
    | h :: t -> List.mem h t || has_duplicate t
  in
  if has_duplicate water then failwith "init: duplicate water coordinates";

  (* Camel must not share a tile with water *)
  if List.mem camel water then
    failwith "init: camel cannot be placed on a water tile";

  (* Build the grid *)
  let grid = Array.init height (fun _ -> Array.make width Blank) in
  List.iter (fun { x; y } -> grid.(y).(x) <- Water) water;
  grid.(camel.y).(camel.x) <- Camel;

  { grid; walls_remaining = walls_available }

let in_bounds (_board : board) (_coord : coordinate) =
  let grid = _board.grid in
  _coord.x < Array.length grid
  && _coord.y < Array.length grid.(0)
  && _coord.x >= 0 && _coord.y >= 0

let get_tile _board _coord = _board.grid.(_coord.x).(_coord.y)
let set_tile _board _coord _tile = _board.grid.(_coord.x).(_coord.y) <- _tile

let check_coord_placement _board _coord =
  if not (in_bounds _board _coord) then Out_of_bounds
  else
    let coord_tile = get_tile _board _coord in
    if coord_tile <> Water && coord_tile <> Wall then Occupied else Ok

let place_wall _board _coord = failwith "TODO: Model.place_wall"
let neighbors4 _board _coord = failwith "TODO: Model.neighbors4"
let reachable_from_camel _board = failwith "TODO: Model.reachable_from_camel"
