open EncloseCamel
open Model
open Parser
open Render
open Raylib

let data_file = "data/basicworld.txt"

let write_default_level_file () =
  let original_file = open_out data_file in
  output_string original_file "2\n";
  output_string original_file "C G G G G G G G G G\n";
  output_string original_file "G G G G G G G G W G\n";
  output_string original_file "G G G G G G G W G G\n";
  output_string original_file "G G G G G G W G G G\n";
  output_string original_file "G G G G W G G G G G\n";
  output_string original_file "G G G G W G G G G G\n";
  output_string original_file "G G W G G G G G G G\n";
  output_string original_file "G W G G G G G G G G\n";
  output_string original_file "G W G G G G G G G G\n";
  output_string original_file "G G G G G G G G G G\n";
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

let board_pixel_width board =
  Array.length board.grid.(0) * Render.gui_tile_px ()

let board_pixel_height board = Array.length board.grid * Render.gui_tile_px ()
let compute_gui_tile_px ?(reserve_tip_footer = 0) board =
  let sw = get_screen_width () in
  let sh = get_screen_height () in
  let rows = Array.length board.grid in
  let cols = Array.length board.grid.(0) in
  let h_pad = 32 in
  let v_pad = 16 in
  let reserve_bottom =
    Render.info_panel_height + 80 + reserve_tip_footer
  in
  let max_by_w = max 1 ((sw - (2 * h_pad)) / cols) in
  let max_by_h = max 1 ((sh - reserve_bottom - v_pad) / rows) in
  min max_by_w max_by_h |> min Render.base_gui_tile_px

let tip_font_size = 14
let tip_line_step = tip_font_size + 5

let tip_wrap_width_px board =
  (* Match board footprint for readability; avoids overflow when centered. *)
  max 96 (board_pixel_width board - 24)

let wrapped_tip_lines_for_board board text =
  let max_px = tip_wrap_width_px board in
  let wrap_paragraph para =
    if para = "" then []
    else
      let raw_words =
        String.split_on_char ' ' para
        |> List.map String.trim
        |> List.filter (fun w -> String.length w > 0)
      in
      let lines_rev, last_line =
        List.fold_left
          (fun ((acc : string list), curr) raw_w ->
            let cand =
              if curr = "" then raw_w else curr ^ " " ^ raw_w
            in
            if measure_text cand tip_font_size <= max_px then (acc, cand)
            else
              match curr with
              | "" ->
                  (* lone word overflows width — still emit as its own row *)
                  (raw_w :: acc, "")
              | _ ->
                  (curr :: acc, raw_w))
          ([], "")
          raw_words
      in
      List.rev
        (match last_line with "" -> lines_rev | _ -> last_line :: lines_rev)
  in
  String.split_on_char '\n' text
  |> List.concat_map wrap_paragraph

let tip_footer_pixel_height board =
  match board.tip with
  | None -> 0
  | Some txt ->
      let lines = wrapped_tip_lines_for_board board txt in
      if lines = [] then 0
      else (
        (* Top gap after Back control + stacked lines + trailing margin *)
        let block = List.length lines * tip_line_step in
        12 + block + 8)

let fit_gui_tile_px_for_viewport board =
  let reserve = ref 0 in
  for _round = 0 to 7 do
    Render.set_gui_tile_px
      (compute_gui_tile_px ~reserve_tip_footer:!reserve board);
    reserve := tip_footer_pixel_height board
  done

let cell_at_mouse board x y =
  let ts = Render.gui_tile_px () in
  let height = board_pixel_height board in
  if y < 0 || y >= height then None
  else Some { r = y / ts; c = x / ts }

let toggle_fullscreen_hotkey () =
  if is_key_pressed Key.F11 then toggle_fullscreen ()

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

let draw_text_info ?(offset_x = 0) board status_message =
  let info_y = board_pixel_height board + 10 in
  
  let walls_text = "Walls remaining: " ^ string_of_int board.walls_remaining in
  let walls_width = measure_text walls_text 20 + 20 in
  let walls_height = 20 in
  draw_rectangle offset_x info_y walls_width walls_height (Color.create 0 0 0 128);
  draw_text walls_text (offset_x + 10 + (walls_width - 20 - measure_text walls_text 20) / 2) (info_y + 2) 20 Color.white;
  
  let status_width = measure_text status_message 18 + 20 in
  let status_height = 20 in
  draw_rectangle offset_x (info_y + 26) status_width status_height (Color.create 0 0 0 128);
  draw_text status_message (offset_x + 10 + (status_width - 20 - measure_text status_message 18) / 2) (info_y + 28) 18 Color.white

let draw_status_panel ?(offset_x = 0) board status_message =
  let width = board_pixel_width board in
  let panel_y = board_pixel_height board in
  draw_rectangle offset_x panel_y width Render.info_panel_height (Color.create 110 155 80 255);
  draw_text_info ~offset_x board status_message

let draw_tip_section ~board_offset_x ~y_top board =
  match board.tip with
  | None -> ()
  | Some txt ->
      let lines = wrapped_tip_lines_for_board board txt in
      if lines = [] then ()
      else
        let tint = Color.create 238 246 229 255 in
        let x0 = board_offset_x + 10 in
        List.iteri
          (fun i line ->
             draw_text line x0 (y_top + (i * tip_line_step)) tip_font_size tint)
          lines
