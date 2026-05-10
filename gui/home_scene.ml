open EncloseCamel
open Raylib
open Scene
open Common

let run_home_scene assets =
  let scale = 0.15 in
  let button_width = int_of_float (float 1480 *. scale) in
  let start_y = 260 in
  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else (
      toggle_fullscreen_hotkey ();
      let window_width = get_screen_width () in
      let window_height = get_screen_height () in
      let x = (window_width - button_width) / 2 in
      let buttons =
        [
          (x, start_y, assets.play, "Play", fun () -> NextScene LevelSelect);
          ( x,
            start_y + 100,
            assets.load_level,
            "Load Level",
            fun () -> NextScene (FileInput ("", true, None)) );
          ( x,
            start_y + 200,
            assets.credits,
            "Credits",
            fun () -> NextScene Credits );
        ]
      in
      begin_drawing ();
      let bg_scale_x =
        float window_width /. float (Texture.width assets.background)
      in
      let bg_scale_y =
        float window_height /. float (Texture.height assets.background)
      in
      draw_texture_ex assets.background (Vector2.create 0.0 0.0) 0.0
        (max bg_scale_x bg_scale_y)
        Color.white;
      draw_rectangle 0 0 window_width window_height (Color.create 0 0 0 153);
      draw_scene_title "Enclose Camel";
      List.iter
        (fun (x, y, texture, label, _) -> draw_button x y texture label scale)
        buttons;
      end_drawing ();
      match find_menu_action buttons scale with
      | Some action -> action ()
      | None -> loop ())
  in
  loop ()
