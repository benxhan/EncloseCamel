open EncloseCamel
open Model
open Raylib

type scene = Home | LevelSelect | GameScene of board | Credits | FileInput of (string * bool * string option)

type scene_outcome = Quit | NextScene of scene

type button_assets = {
  play : Texture.t;
  load_level : Texture.t;
  credits : Texture.t;
  back : Texture.t;
  background : Texture.t;
}

let button_textures : button_assets option ref = ref None

let asset_dir = "gui/assets"

let button_texture_path file_name = Filename.concat asset_dir file_name

let load_button_texture file_name = load_texture (button_texture_path file_name)

let load_menu_textures () =
  match !button_textures with
  | Some assets -> assets
  | None ->
      let assets =
        {
          play = load_button_texture "play_button.png";
          load_level = load_button_texture "load_level_button.png";
          credits = load_button_texture "credits_button.png";
          back = load_button_texture "back_button.png";
          background = load_button_texture "home_page_background.png";
        }
      in
      button_textures := Some assets;
      assets

let unload_menu_textures () =
  match !button_textures with
  | None -> ()
  | Some assets ->
      unload_texture assets.play;
      unload_texture assets.load_level;
      unload_texture assets.credits;
      unload_texture assets.back;
      unload_texture assets.background;
      button_textures := None

let button_rect x y texture scale =
  Rectangle.create
    (float x)
    (float y)
    (float (Texture.width texture) *. scale)
    (float (Texture.height texture) *. scale)

let mouse_over rect =
  let mx = float (get_mouse_x ()) in
  let my = float (get_mouse_y ()) in
  mx >= Rectangle.x rect
  && mx <= Rectangle.x rect +. Rectangle.width rect
  && my >= Rectangle.y rect
  && my <= Rectangle.y rect +. Rectangle.height rect

let button_clicked rect = mouse_over rect && is_mouse_button_pressed MouseButton.Left

let draw_button x y texture label scale =
  draw_texture_ex texture (Vector2.create (float x) (float y)) 0.0 scale Color.white;
  let scaled_w = int_of_float (float (Texture.width texture) *. scale) in
  let scaled_h = int_of_float (float (Texture.height texture) *. scale) in
  draw_rectangle_lines x y scaled_w scaled_h Color.black;
  let text_width = measure_text label 20 in
  draw_text
    label
    (x + (scaled_w - text_width) / 2)
    (y + (scaled_h - 20) / 2)
    20 Color.black

let draw_scene_title title =
  draw_text title 280 100 60 Color.black;
  draw_text
    "Use the buttons below to navigate between scenes."
    200 180 24 Color.black

let find_menu_action buttons scale =
  match
    List.find_opt
      (fun (x, y, texture, _, _) -> button_clicked (button_rect x y texture scale))
      buttons
  with
  | Some (_, _, _, _, action) -> Some action
  | None -> None
