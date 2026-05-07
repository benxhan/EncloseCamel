open Raylib
open Scene

let run_credits_scene assets =
  let button_x = 360 in
  let button_y = 430 in
  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then NextScene Home
    else (
      begin_drawing ();
      clear_background Color.raywhite;
      draw_scene_title "Credits";
      draw_text "Developed for CS 3110 final project" 190 220 24 Color.black;
      draw_text "Team: Yummylanders" 310 260 24 Color.black;
      draw_button button_x button_y assets.back "Back";
      end_drawing ();
      if button_clicked (button_rect button_x button_y assets.back) then NextScene Home
      else loop ())
  in
  loop ()
