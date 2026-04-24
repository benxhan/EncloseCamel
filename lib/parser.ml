type parse_error =
  | Empty_input
  | Bad_format
  | Not_an_int of string

let parse_coordinate input =
  let s = String.trim input in
  if s = "" then Error Empty_input
  else
    let len = String.length s in
    if len < 5 || s.[0] <> '[' || s.[len - 1] <> ']' then Error Bad_format
    else
      let inner = String.sub s 1 (len - 2) in
      let coords = String.split_on_char ',' inner in
      match coords with
      | [ x; y ] -> (
          let x_coord = String.trim x in
          let y_coord = String.trim y in
          if x_coord = "" || y_coord = "" then Error Bad_format
          else
            match int_of_string_opt x with
            | None -> Error (Not_an_int x)
            | Some x -> (
                match int_of_string_opt y with
                | None -> Error (Not_an_int y)
                | Some y -> Ok { Model.x; y }))
      | _ -> Error Bad_format

let parse_error_to_string = function
  | Empty_input -> "Input is empty. Enter a coordinate like [x,y]."
  | Bad_format -> "Bad coordinate format. Use [x,y]."
  | Not_an_int token ->
      "Invalid integer: \"" ^ token ^ "\". Use [x,y] with integers."

let load_board filename =
  (* Read all lines *)
  let lines =
    let ic = open_in filename in
    let rec collect acc =
      match input_line ic with
      | line -> collect (line :: acc)
      | exception End_of_file -> close_in ic; List.rev acc
    in
    collect []
  in

  (* Strip away spaces and fully empty lines *)
  let rows =
    lines
    |> List.map (String.concat "" << String.split_on_char ' ')
    |> List.filter (fun s -> String.length s > 0)
  in
  if rows = [] then failwith "load_board: file is empty"

  (* Check rectangularity *)
  let width = String.length (List.hd rows) in
  List.iteri (fun i row -> if String.length row <> width then
      failwith (Printf.sprintf
      "load_board: non-rectangular grid (row %d has $d tiles, expected $d)"
      i (String.length row) width)
      ) rows;
  let height = list.length rows in

  (* Map characters to tiles *)
  let camel_pos = ref None in
  let wall_count = ref 0 in

  let char_to_tile row_idx col_idx ch =
    match ch with
    | 'C' ->
      (match !camel_pos with
      | Some _ -> failwith "load_board: multiple camel tiles found"
      | None -> camel_pos := Some {Model.x = col_idx; y = row_idx});
      Model.Camel
    | 'W' -> Model.Water
    | 'B' -> incr wall_count; Model.Wall
    | 'G' -> Model.Blank
    | c -> failwith (Printf.sprintf "load_board: invalid character '%c' at row %d, col %d
      c row_idx col_idx") in

  let grid : Model.tile array array =
    rows 
    |> List.mapi (fun row_idx row ->
      Array.init width (fun col_idx ->
        char_to_tile row_idx col_idx row.[col_idx]))
    |> Array.of_list
      in

    (* Validate the camel *)

    let camel = 
      match !camel_pos with
      | None -> failwith "load_board: no camel tile found"
      | Some c -> c
    in