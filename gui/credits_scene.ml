open Raylib
open Scene

let run_credits_scene assets =
  let scale = 0.15 in
  let window_width = get_screen_width () in
  let window_height = get_screen_height () in
  let margin = 96 in
  let button_width = int_of_float (float 1480 *. scale) in
  let button_x = (window_width - button_width) / 2 in
  let button_y = window_height - 100 in
  let credits_text =
    "Thanks for playing Enclose Camel by Maxwell Li, Daniel Lee, Ben Han, \
     Caleb Helsel, and Jiayi Bai!\n\n\
     Also huge thanks to our project mentor Sophie Cheng and project grader \
     Sophia Pan, and our CS 3110 professors Dexter Kozen and Ayaka \
     Yorihiro!!!\n\n\
     We hope you enjoyed the game and had fun playing it as much as we had fun \
     making it!"
  in
  let font_size = 24 in
  let line_height = 34 in
  let max_text_width = window_width - (2 * margin) in
  let wrap_line line =
    let words = String.split_on_char ' ' line in
    let rec aux current acc = function
      | [] -> List.rev (current :: acc)
      | word :: rest ->
          let next_line = if current = "" then word else current ^ " " ^ word in
          if measure_text next_line font_size <= max_text_width then
            aux next_line acc rest
          else if current = "" then aux word acc rest
          else aux word (current :: acc) rest
    in
    aux "" [] words
  in
  let wrapped_lines =
    credits_text |> String.split_on_char '\n' |> List.map wrap_line
    |> List.flatten
  in
  let initial_text_y = float_of_int (window_height + margin) in
  let scroll_speed = 40.0 in
  let fade_duration = 0.75 in
  let rec draw_lines lines y =
    match lines with
    | [] -> ()
    | line :: rest ->
        draw_text_hcenter line y font_size Color.white;
        draw_lines rest (y + line_height)
  in
  let rec loop y_offset fade_alpha =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then
      NextScene Home
    else
      let frame_time = get_frame_time () in
      let fade_started = y_offset <= 0.0 in
      let fade_alpha =
        if fade_started then
          min 255.0 (fade_alpha +. (255.0 *. frame_time /. fade_duration))
        else fade_alpha
      in
      let text_y = int_of_float y_offset in
      let bg_scale_x =
        float window_width /. float (Texture.width assets.background)
      in
      let bg_scale_y =
        float window_height /. float (Texture.height assets.background)
      in
      begin_drawing ();
      draw_texture_ex assets.background (Vector2.create 0.0 0.0) 0.0
        (max bg_scale_x bg_scale_y)
        Color.white;
      draw_rectangle 0 0 window_width window_height (Color.create 0 0 0 153);
      draw_scene_title "Credits";
      draw_lines wrapped_lines text_y;
      draw_button button_x button_y assets.back "Home" scale;
      if fade_alpha > 0.0 then
        draw_rectangle 0 0 window_width window_height
          (Color.create 0 0 0 (int_of_float fade_alpha));
      end_drawing ();
      if button_clicked (button_rect button_x button_y assets.back scale) then
        NextScene Home
      else if fade_started && fade_alpha >= 255.0 then NextScene Home
      else loop (y_offset -. (scroll_speed *. frame_time)) fade_alpha
  in
  loop initial_text_y 0.0
