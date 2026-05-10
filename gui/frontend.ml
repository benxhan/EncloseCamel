open EncloseCamel
open Raylib
open Scene
open Common
open Render
open Home_scene
open Level_select_scene
open Game_scene
open Credits_scene
open File_input_scene

let run_scene assets scene =
  match scene with
  | Home -> run_home_scene assets
  | LevelSelect -> run_level_select_scene assets
  | GameScene (board, level_index) -> run_game_scene assets board level_index
  | Credits -> run_credits_scene assets
  | FileInput state -> run_file_input_scene assets state

let () =
  set_config_flags [ Window_resizable ];
  (* Game board scales inside the viewport; menus use full window. *)
  let width = 1024 in
  let height = 768 in
  init_window width height "Enclose Camel GUI";
  set_target_fps 60;
  init_gui_textures ();
  let assets = load_menu_textures () in
  let rec loop scene =
    match run_scene assets scene with
    | Quit -> ()
    | NextScene next -> loop next
  in
  loop Home;
  unload_menu_textures ();
  unload_gui_textures ();
  close_window ();
  print_endline "Goodbye!"
