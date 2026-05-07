open Raylib
open Scene

let run_credits_scene assets =
  let window_width = get_screen_width () in
  let window_height = get_screen_height () in
  let button_x = (window_width - Texture.width assets.back) / 2 in
  let button_y = window_height - 100 in
  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then NextScene Home
    else (
      begin_drawing ();
      clear_background Color.raywhite;
      draw_scene_title "Credits";
      draw_text "Credits coming soon!" ((window_width - measure_text "Credits coming soon!" 24) / 2) 220 24 Color.black;
      draw_button button_x button_y assets.back "Back";
      end_drawing ();
      if button_clicked (button_rect button_x button_y assets.back) then NextScene Home
      else loop ())
  in
  loop ()
