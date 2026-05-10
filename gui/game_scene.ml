open EncloseCamel
open Model
open Raylib
open Scene
open Common
open Render

let level_files = [| "data/level1.txt"; "data/level2.txt"; "data/level3.txt" |]

let can_go_next board =
  match reachable_from_camel board with
  | Enclosed _ -> true
  | Open -> false

let load_next_level level_index =
  let next_index = level_index + 1 in
  if next_index >= Array.length level_files then LevelSelect
  else
    try GameScene (Parser.load_board level_files.(next_index), next_index)
    with exn ->
      print_endline ("Could not load next level: " ^ Printexc.to_string exn);
      LevelSelect

let run_game_scene assets board level_index =
  let scale = 0.15 in
  let layout board =
    fit_gui_tile_px_for_viewport board;
    let window_width = get_screen_width () in
    let board_width = board_pixel_width board in
    let board_offset_x = (window_width - board_width) / 2 in
    let back_button_x = board_offset_x + 10 in
    let back_button_y =
      board_pixel_height board + Render.info_panel_height + 10
    in
    let button_width =
      int_of_float (float (Texture.width assets.back) *. scale)
    in
    let button_height =
      int_of_float (float (Texture.height assets.back) *. scale)
    in
    let back_rect = button_rect back_button_x back_button_y assets.back scale in
    ( board_offset_x,
      back_button_x,
      back_button_y,
      button_width,
      button_height,
      back_rect )
  in
  let rec loop board status_message =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else if is_key_pressed Key.Escape then NextScene Home
    else (
      toggle_fullscreen_hotkey ();
      let ( board_offset_x,
            back_button_x,
            back_button_y,
            button_width,
            button_height,
            back_rect ) =
        layout board
      in
      let level_complete = can_go_next board in

      let next_button_x = back_button_x + button_width + 20 in
      let next_button_y = back_button_y in
      let next_rect =
        Rectangle.create
          (float_of_int next_button_x)
          (float_of_int next_button_y)
          (float_of_int button_width)
          (float_of_int button_height)
      in

      let next_label =
        if level_index + 1 < Array.length level_files then "Next Level"
        else "Levels"
      in
      begin_drawing ();
      clear_background (Color.create 110 155 80 255);
      draw_board_gui ~offset_x:board_offset_x board;
      draw_status_panel ~offset_x:board_offset_x board status_message;
      draw_rectangle back_button_x back_button_y button_width button_height
        (Color.create 110 155 80 255);
      draw_rectangle_lines back_button_x back_button_y button_width
        button_height Color.black;
      let text_width = measure_text "Back" 20 in
      draw_text "Back"
        (back_button_x + ((button_width - text_width) / 2))
        (back_button_y + ((button_height - 20) / 2))
        20 Color.white;
      if level_complete then begin
        draw_rectangle next_button_x next_button_y button_width button_height
          (Color.create 110 155 80 255);
        draw_rectangle_lines next_button_x next_button_y button_width
          button_height Color.black;
        let next_text_width = measure_text next_label 20 in
        draw_text next_label
          (next_button_x + ((button_width - next_text_width) / 2))
          (next_button_y + ((button_height - 20) / 2))
          20 Color.white
      end;
      draw_tip_section ~board_offset_x
        ~y_top:(back_button_y + button_height + 12)
        board;
      end_drawing ();
      if button_clicked back_rect then NextScene Home
      else if level_complete && button_clicked next_rect then
        NextScene (load_next_level level_index)
      else if is_mouse_button_pressed MouseButton.Left then
        let x = get_mouse_x () - board_offset_x in
        let y = get_mouse_y () in
        match cell_at_mouse board x y with
        | Some coord ->
            let next_board, msg =
              placement_message board coord (place_wall board coord)
            in
            loop next_board msg
        | None -> loop board status_message
      else loop board status_message)
  in
  loop board "Click a tile to place or remove a wall."
