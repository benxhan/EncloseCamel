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
            match int_of_string_opt r_coord with
            | None -> Error (Not_an_int r_coord)
            | Some r -> (
                match int_of_string_opt c_coord with
                | None -> Error (Not_an_int c_coord)
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

  (* Split metadata lines vs optional TIP footer vs grid *)
  let walls_remaining, max_score, after_header_lines =
    match lines with
    | [] -> failwith "load_board: file is empty"
    | hd :: tl -> (
        match int_of_string_opt (String.trim hd) with
        | None ->
            failwith
              (Printf.sprintf
                 "load_board: first line must be walls_remaining, got %S" hd)
        | Some walls -> (
            match tl with
            | [] -> failwith "load_board: missing max_score on second line"
            | hd2 :: rest -> (
                match int_of_string_opt (String.trim hd2) with
                | None ->
                    failwith
                      (Printf.sprintf
                         "load_board: second line must be max_score, got %S" hd2)
                | Some score -> (walls, score, rest))))
  in

  let grid_raw_lines_no_tip, tip_opt =
    let is_tip_marker_line line =
      match String.lowercase_ascii (String.trim line) with
      | "tip" | "tips" -> true
      | _ -> false
    in
    let rec take_grid acc = function
      | [] -> (List.rev acc, None)
      | line :: rest when is_tip_marker_line line ->
          let merged =
            match rest with
            | [] -> ""
            | _ -> String.concat "\n" rest
          in
          let s = String.trim merged in
          if s = "" then (List.rev acc, None)
          else (List.rev acc, Some s)
      | line :: rest -> take_grid (line :: acc) rest
    in
    take_grid [] after_header_lines
  in

  (* Strip spaces and blank lines inside the tile grid *)
  let rows =
    grid_raw_lines_no_tip
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
    | '0' .. '9' as d -> Model.Portal (int_of_char d - int_of_char '0')
    | c -> (
        let tile_opt =
          List.find_opt
            (fun t -> (Model.properties_of t).file_char = c)
            Model.base_tiles
        in
        match tile_opt with
        | Some Model.Camel ->
            (match !camel_pos with
            | Some _ -> failwith "load_board: multiple camel tiles found"
            | None -> camel_pos := Some { Model.r = row_idx; c = col_idx });
            Model.Camel
        | Some t -> t
        | None ->
            failwith
              (Printf.sprintf
                 "load_board: invalid character '%c' at row %d, col %d" c
                 row_idx col_idx))
  in
  let grid : Model.tile array array =
    rows
    |> List.mapi (fun row_idx row ->
        Array.init width (fun col_idx ->
            char_to_tile row_idx col_idx row.[col_idx]))
    |> Array.of_list
  in
  (* Validate and unwrap camel *)
  (* Validate and unwrap camel *)
  let camel_loc =
    match !camel_pos with
    | None -> failwith "load_board: no camel tile found"
    | Some coord -> coord
  in
  (* Assemble the board *)
  { Model.grid; walls_remaining; max_score; camel_loc; tip = tip_opt }
