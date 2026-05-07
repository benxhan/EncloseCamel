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
  | GameScene board -> run_game_scene assets board
  | Credits -> run_credits_scene assets
  | FileInput state -> run_file_input_scene assets state

let () =
  let board = load_starting_board () in
  let width = max 960 (board_pixel_width board) in
  let height = max 768 (window_height board) in
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
