open EncloseCamel
open Model
open Raylib
open Scene
open Common
open Render

let run_game_scene assets board =
  let back_button_x = 10 in
  let back_button_y = board_pixel_height board + 10 in
  let back_rect = button_rect back_button_x back_button_y assets.back in
  let rec loop board status_message =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else if is_key_pressed Key.Escape then NextScene Home
    else (
      begin_drawing ();
      clear_background Color.raywhite;
      draw_board_gui board;
      draw_status_panel board status_message;
      draw_button back_button_x back_button_y assets.back "Back";
      end_drawing ();
      if button_clicked back_rect then NextScene Home
      else if is_mouse_button_pressed MouseButton.Left then
        let x = get_mouse_x () in
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
