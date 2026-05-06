open EncloseCamel
open Model
open Parser
open Render
open Raylib

let data_file = "data/basicworld.txt"

let write_default_level_file () =
  let original_file = open_out data_file in
  output_string original_file
    "2\n\
     C G G G G G G G G G\n\
     G G G G G G G G W G\n\
     G G G G G G G W G G\n\
     G G G G G G W G G G\n\
     G G G G W G G G G G\n\
     G G G G W G G G G G\n\
     G G W G G G G G G G\n\
     G W G G G G G G G G\n\
     G W G G G G G G G G\n\
     G G G G G G G G G G\n";
  close_out original_file

let load_default_board () =
  if not (Sys.file_exists data_file) then (
    print_endline "Default board file not found. Creating default board.";
    write_default_level_file ());
  try Parser.load_board data_file
  with exn ->
    print_endline
      ("Default board is invalid. Recreating board: " ^ Printexc.to_string exn);
    write_default_level_file ();
    load_board data_file

let load_starting_board () =
  match Array.to_list Sys.argv with
  | [ _program ] ->
      print_endline "No level file provided. Using default board.";
      print_endline
        "To input a board, use: dune exec gui/frontend.exe -- <level-file>";
      load_default_board ()
  | [ _program; filename ] -> (
      try
        print_endline ("Loading level file: " ^ filename);
        Parser.load_board filename
      with
      | Sys_error msg ->
          print_endline ("Could not load level file: " ^ msg);
          print_endline "Using default board instead.";
          load_default_board ()
      | Failure msg ->
          print_endline ("Error reading file: " ^ msg);
          print_endline "Using default board instead.";
          load_default_board ()
      | exn ->
          print_endline
            ("Unexpected error while reading file: " ^ Printexc.to_string exn);
          print_endline "Using default board instead.";
          load_default_board ())
  | _ ->
      print_endline "Unexpected input. Using default board instead.";
      print_endline
        "To input a board, use: dune exec gui/frontend.exe -- <level-file>";
      load_default_board ()

let board_pixel_width board = Array.length board.grid.(0) * Render.tile_size

let board_pixel_height board = Array.length board.grid * Render.tile_size

let window_height board = board_pixel_height board + Render.info_panel_height

let cell_at_mouse board x y =
  let height = board_pixel_height board in
  if y < 0 || y >= height then None
  else
    Some
      {
        r = y / Render.tile_size;
        c = x / Render.tile_size;
      }

let placement_message board coord result =
  match result with
  | Result.Ok next_board -> (
      match reachable_from_camel next_board with
      | Open -> (next_board, "Placed rock.")
      | Enclosed { score; _ } ->
          if score = next_board.max_score then
            ( next_board,
              "You won! Max score of " ^ string_of_int score ^ " achieved." )
          else (next_board, "Score: " ^ string_of_int score))
  | Result.Error bad_move ->
      let message =
        match bad_move with
        | Out_of_bounds -> "Coordinates out of bounds! Try again"
        | Occupied -> "This spot is occupied! Try again"
        | Ok -> ""
      in
      (board, message)

let draw_text_info board status_message =
  let info_y = board_pixel_height board + 10 in
  draw_text
    ("Camel: (" ^ string_of_int board.camel_loc.r ^ ", "
    ^ string_of_int board.camel_loc.c ^ ")")
    10 info_y 20 Color.black;
  draw_text
    ("Walls remaining: " ^ string_of_int board.walls_remaining)
    10 (info_y + 26) 20 Color.black;
  draw_text status_message 10 (info_y + 52) 18 Color.darkgray;
  draw_text
    "Left click blank tile to place/remove a wall. Press ESC or Q to quit."
    10 (info_y + 76) 16 Color.gray

let draw_status_panel board status_message =
  let width = board_pixel_width board in
  let panel_y = board_pixel_height board in
  draw_rectangle 0 panel_y width Render.info_panel_height Color.raywhite;
  draw_rectangle_lines 0 panel_y width Render.info_panel_height Color.black;
  draw_text_info board status_message

let rec run_game board status_message =
  if window_should_close () || is_key_pressed Key.Escape || is_key_pressed Key.Q
  then board
  else (
    if is_mouse_button_pressed MouseButton.Left then
      let x = get_mouse_x () in
      let y = get_mouse_y () in
      match cell_at_mouse board x y with
      | Some coord ->
          let next_board, msg = placement_message board coord (place_wall board coord) in
          render_game next_board msg
      | None -> render_game board status_message
    else render_game board status_message)

and render_game board status_message =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_board_gui board;
  draw_status_panel board status_message;
  end_drawing ();
  run_game board status_message

let () =
  let board = load_starting_board () in
  let width = board_pixel_width board in
  let height = window_height board in
  init_window width height "Enclose Camel GUI";
  set_target_fps 60;
  init_gui_textures ();
  let _final_board = run_game board "Click a tile to place or remove a wall." in
  unload_gui_textures ();
  close_window ();
  print_endline "Goodbye!"
