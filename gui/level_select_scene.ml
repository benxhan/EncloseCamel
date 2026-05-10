open EncloseCamel
open Raylib
open Scene
open Common
open EncloseCamel.Model

(* All the level files that we have *)
let level_files = [| "data/level1.txt"; "data/level2.txt"; "data/level3.txt" |]

let preview_color = function
  | Camel -> Color.create 235 196 67 255
  | Water -> Color.create 70 140 255 255
  | Wall -> Color.create 70 70 70 255
  | Blank -> Color.create 120 190 90 255
  | Cherry -> Color.create 220 50 50 255
  | Bees -> Color.create 240 210 70 255
  | GoldenApple -> Color.create 255 165 0 255
  | LavaBucket -> Color.create 140 80 40 255
  | Portal _ -> Color.create 160 90 220 255

let draw_level_preview board x y w h =
  let rows = Array.length board.grid in
  let cols = Array.length board.grid.(0) in
  let cell = max 2 (min (w / cols) (h / rows)) in
  let preview_w = cols * cell in
  let preview_h = rows * cell in
  let ox = x + ((w - preview_w) / 2) in
  let oy = y + ((h - preview_h) / 2) in

  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c tile ->
          let px = ox + (c * cell) in
          let py = oy + (r * cell) in
          draw_rectangle px py cell cell (preview_color tile);
          if cell >= 8 then
            draw_rectangle_lines px py cell cell (Color.create 0 0 0 40))
        row)
    board.grid;

  draw_rectangle_lines ox oy preview_w preview_h Color.black

let load_level filename =
  try GameScene (Parser.load_board filename)
  with exn ->
    print_endline ("Could not load level: " ^ Printexc.to_string exn);
    LevelSelect

let run_level_select_scene assets =
  let mouse_ready = ref false in
  let accept_clicks = ref false in
  let level_previews =
    Array.map
      (fun filename -> try Some (Parser.load_board filename) with _ -> None)
      level_files
  in

  let cols = 5 in
  let button_width = 150 in
  let button_height = 115 in
  let gap_x = 24 in
  let gap_y = 135 in

  let mk_layout window_width =
    let total_width = (cols * button_width) + ((cols - 1) * gap_x) in
    let start_x = (window_width - total_width) / 2 in
    let start_y = 180 in
    let level_buttons =
      List.init (Array.length level_files) (fun i ->
          let col = i mod cols in
          let row = i / cols in
          let x = start_x + (col * (button_width + gap_x)) in
          let y = start_y + (row * gap_y) in
          let label = "Level " ^ string_of_int (i + 1) in
          let filename = level_files.(i) in
          (i, x, y, label, fun () -> load_level filename))
    in
    let back_button =
      [
        ( start_x,
          start_y + (2 * gap_y) + 20,
          assets.back,
          "Back",
          fun () -> Home );
      ]
    in
    (level_buttons, back_button)
  in
  let draw_preview_button i x y label =
    let rect =
      Rectangle.create (float_of_int x) (float_of_int y)
        (float_of_int button_width)
        (float_of_int button_height)
    in

    let hovered = check_collision_point_rec (get_mouse_position ()) rect in
    let bg =
      if hovered then Color.create 230 230 230 255
      else Color.create 200 200 200 255
    in

    draw_rectangle_rec rect bg;
    draw_rectangle_lines x y button_width button_height Color.black;

    begin match level_previews.(i) with
    | Some board ->
        draw_level_preview board (x + 8) (y + 8) (button_width - 16) 78
    | None -> draw_text "Missing" (x + 34) (y + 35) 20 Color.red
    end;

    draw_text label (x + 35) (y + 90) 20 Color.black;

    !accept_clicks && hovered && is_mouse_button_pressed MouseButton.Left
  in

  let rec loop () =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q || is_key_pressed Key.Escape then
      NextScene Home
    else (
      toggle_fullscreen_hotkey ();
      let window_width = get_screen_width () in
      let window_height = get_screen_height () in
      let level_buttons, back_button = mk_layout window_width in
      accept_clicks := !mouse_ready;

      if (not !mouse_ready) && not (is_mouse_button_down MouseButton.Left) then
        mouse_ready := true;
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
      draw_scene_title "Level Select";

      let clicked_level = ref None in

      List.iter
        (fun (i, x, y, label, action) ->
          if draw_preview_button i x y label then clicked_level := Some action)
        level_buttons;

      List.iter
        (fun (x, y, texture, label, _) -> draw_button x y texture label 0.09)
        back_button;

      end_drawing ();

      match !clicked_level with
      | Some action -> NextScene (action ())
      | None -> (
          match find_menu_action back_button 0.09 with
          | Some action -> NextScene (action ())
          | None -> loop ()))
  in
  loop ()
