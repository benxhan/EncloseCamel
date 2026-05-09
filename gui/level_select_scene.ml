open Raylib
open Scene
open Common

let run_level_select_scene assets =
  let scale = 0.15 in
  let window_width = get_screen_width () in
  let window_height = get_screen_height () in
  let button_width = int_of_float (float 1480 *. scale) in
  let x = (window_width - button_width) / 2 in
  let start_y = 260 in
  let buttons =
    [ (x, start_y, assets.play, "Start Default", fun () -> GameScene (load_default_board ()))
    ; (x, start_y + 120, assets.back, "Back", fun () -> Home)
    ]
  in
  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then NextScene Home
    else (
      begin_drawing ();
      let bg_scale_x = float window_width /. float (Texture.width assets.background) in
      let bg_scale_y = float window_height /. float (Texture.height assets.background) in
      draw_texture_ex assets.background (Vector2.create 0.0 0.0) 0.0
        (max bg_scale_x bg_scale_y)
        Color.white;
      draw_rectangle 0 0 window_width window_height (Color.create 0 0 0 153);
      draw_scene_title "Level Select";
      List.iter
        (fun (x, y, texture, label, _) -> draw_button x y texture label scale)
        buttons;
      end_drawing ();
      match find_menu_action buttons scale with
      | Some action -> NextScene (action ())
      | None -> loop ())
  in
  loop ()
