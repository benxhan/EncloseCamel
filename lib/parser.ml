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
      | [ r; c ] -> (
          let r_coord = String.trim r in
          let c_coord = String.trim c in
          if r_coord = "" || c_coord = "" then Error Bad_format
          else
            match int_of_string_opt r with
            | None -> Error (Not_an_int r)
            | Some r -> (
                match int_of_string_opt c with
                | None -> Error (Not_an_int c)
                | Some c -> Ok { Model.r; c }))
      | _ -> Error Bad_format

let parse_error_to_string = function
  | Empty_input -> "Input is empty. Enter a coordinate like [r,c]."
  | Bad_format -> "Bad coordinate format. Use [r,c]."
  | Not_an_int token ->
      "Invalid integer: \"" ^ token ^ "\". Use [r,c] with integers."

let load_board (filename : string) : Model.board =
  (* Intake lines *)
  let lines =
    let ic = open_in filename in
    let rec collect acc =
      match input_line ic with
      | line -> collect (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    collect []
  in

  (* Split walls remaining from grid lines *)
  let walls_remaining, grid_lines =
    match lines with
    | [] -> failwith "load_board: file is empty"
    | hd :: tl -> (
        match int_of_string_opt (String.trim hd) with
        | None ->
            failwith
              (Printf.sprintf
                 "load_board: first line must be walls_remaining, got %S" hd)
        | Some n -> (n, tl))
  in

  (* Strip away spaces and empty lines *)
  let rows =
    grid_lines
    |> List.map (fun s -> String.split_on_char ' ' s |> String.concat "")
    |> List.filter (fun s -> String.length s > 0)
  in
  if rows = [] then failwith "load_board: file contains no grid rows";

  (* Check for basic rectangularity *)
  let width = String.length (List.hd rows) in
  List.iteri
    (fun i row ->
      let len = String.length row in
      if len <> width then
        failwith
          (Printf.sprintf
             "load_board: non-rectangular grid (row %d has %d tiles, expected \
              %d)"
             i len width))
    rows;

  (* Map Characters to tiles *)
  let camel_pos = ref None in

  let char_to_tile row_idx col_idx ch =
    match ch with
    | 'C' ->
        (match !camel_pos with
        | Some _ -> failwith "load_board: multiple camel tiles found"
        | None -> camel_pos := Some { Model.r = col_idx; c = row_idx });
        Model.Camel
    | 'W' -> Model.Water
    | 'B' -> Model.Wall
    | 'G' -> Model.Blank
    | c ->
        failwith
          (Printf.sprintf "load_board: invalid character '%c' at row %d, col %d"
             c row_idx col_idx)
  in

  let grid : Model.tile array array =
    rows
    |> List.mapi (fun row_idx row ->
        Array.init width (fun col_idx ->
            char_to_tile row_idx col_idx row.[col_idx]))
    |> Array.of_list
  in

  (* Validate camel *)
  (match !camel_pos with
  | None -> failwith "load_board: no camel tile found"
  | Some _ -> ());

  (* Assemble the board *)
  { Model.grid; walls_remaining }
