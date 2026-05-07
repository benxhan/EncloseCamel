open EncloseCamel
open Raylib
open Scene
open Common

let run_home_scene assets =
  let window_width = get_screen_width () in
  let x = (window_width - 260) / 2 in
  let start_y = 260 in
  let buttons =
    [ (x, start_y, assets.play, "Play", fun () -> NextScene LevelSelect)
    ; (x, start_y + 100, assets.load_level, "Load Level", fun () -> NextScene (FileInput ("", true, None)))
    ; (x, start_y + 200, assets.credits, "Credits", fun () -> NextScene Credits)
    ]
  in
  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else (
      begin_drawing ();
      clear_background Color.raywhite;
      draw_scene_title "Enclose Camel";
      List.iter
        (fun (x, y, texture, label, _) -> draw_button x y texture label)
        buttons;
      end_drawing ();
      match find_menu_action buttons with
      | Some action -> action ()
      | None -> loop ())
  in
  loop ()
