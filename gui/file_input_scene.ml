open EncloseCamel
open Model
open Parser
open Raylib
open Scene

let run_file_input_scene assets state =
  let scale = 0.15 in
  let window_width = get_screen_width () in
  let window_height = get_screen_height () in
  let title_y = window_height / 4 in
  let input_box_width = 400 in
  let input_box_height = 40 in
  let prompt_y = title_y + 80 in
  let input_box_x = (window_width - input_box_width) / 2 in
  let input_box_y = prompt_y + 40 in
  let button_width = int_of_float (float 1480 *. scale) in
  let button_x = (window_width - button_width) / 2 in
  let load_button_y = input_box_y + 60 in
  let back_button_y = load_button_y + 120 in
  let load_rect = button_rect button_x load_button_y assets.load_level scale in
  let back_rect = button_rect button_x back_button_y assets.back scale in

  let loop (input_text, cursor_visible, error_message) =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then NextScene Home
    else (
      let max_text_width = input_box_width - 20 in
      let new_input_text, should_load, new_error_message =
        let ctrl_or_cmd_down () =
          is_key_down Key.Left_control
          || is_key_down Key.Right_control
          || is_key_down Key.Left_super
          || is_key_down Key.Right_super
        in
        let paste_text text =
          match get_clipboard_text () with
          | Some clipboard -> text ^ clipboard
          | None -> text
        in
        let rec handle_keys text =
          if is_key_pressed Key.Backspace && String.length text > 0 then
            (String.sub text 0 (String.length text - 1), false)
          else if is_key_pressed Key.Enter then (text, true)
          else if ctrl_or_cmd_down () && is_key_pressed Key.V then
            (paste_text text, false)
          else
            let char_code = Uchar.to_int (get_char_pressed ()) in
            if char_code >= 32 && char_code <= 126 then
              (text ^ String.make 1 (Char.chr char_code), false)
            else (text, false)
        in
        let text, should_load = handle_keys input_text in
        let error_message = if text <> input_text then None else error_message in
        (text, should_load, error_message)
      in
      let visible_text =
        if measure_text new_input_text 20 <= max_text_width then new_input_text
        else
          let len = String.length new_input_text in
          let rec drop_left start =
            let slice = String.sub new_input_text start (len - start) in
            if measure_text slice 20 <= max_text_width then slice
            else drop_left (start + 1)
          in
          drop_left 1
      in
      let new_cursor_visible = (get_time () *. 2.0 |> int_of_float) mod 2 = 0 in
      begin_drawing ();
      let bg_scale_x = float window_width /. float (Texture.width assets.background) in
      let bg_scale_y = float window_height /. float (Texture.height assets.background) in
      draw_texture_ex assets.background (Vector2.create 0.0 0.0) 0.0
        (max bg_scale_x bg_scale_y)
        Color.white;
      draw_rectangle 0 0 window_width window_height (Color.create 0 0 0 153);
      let title_text = "Load Level" in
      let title_width = measure_text title_text 60 in
      draw_text title_text ((window_width - title_width) / 2) title_y 60 Color.white;
      let prompt_text, prompt_color =
        match new_error_message with
        | Some msg -> (msg, Color.red)
        | None -> ("Enter the path to the level file:", Color.white)
      in
      let prompt_x = (window_width - measure_text prompt_text 20) / 2 in
      draw_text prompt_text prompt_x prompt_y 20 prompt_color;
      draw_rectangle input_box_x input_box_y input_box_width input_box_height Color.white;
      draw_rectangle_lines input_box_x input_box_y input_box_width input_box_height Color.black;
      draw_text visible_text (input_box_x + 10) (input_box_y + 10) 20 Color.white;
      if new_cursor_visible then begin
        let cursor_x = input_box_x + 10 + measure_text visible_text 20 in
        draw_line cursor_x (input_box_y + 5) cursor_x (input_box_y + input_box_height - 5) Color.white
      end;
      draw_button button_x load_button_y assets.load_level "Load" scale;
      draw_button button_x back_button_y assets.back "Back" scale;
      end_drawing ();
      if should_load || button_clicked load_rect then
        (try
           let board = Parser.load_board new_input_text in
           NextScene (GameScene board)
         with
         | Sys_error _
         | Failure _ ->
             NextScene
               (FileInput
                  ( new_input_text
                  , new_cursor_visible
                  , Some
                      "Invalid level file. Check the path and file format." )))
      else if button_clicked back_rect then
        NextScene Home
      else
        NextScene (FileInput (new_input_text, new_cursor_visible, new_error_message))
    )
  in
  loop state
