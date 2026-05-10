open EncloseCamel
open Model
open Raylib
open Scene
open Common
open Render

let run_game_scene assets board =
  let scale = 0.15 in
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
  let rec loop board status_message =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else if is_key_pressed Key.Escape then NextScene Home
    else (
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
      end_drawing ();
      if button_clicked back_rect then NextScene Home
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
