open EncloseCamel
open Model
open Raylib
open Scene
open Common
open Render

(* All the level files that we have, also introduced in level_select_scene. *)
let level_files =
  [|
    "data/level1.txt";
    "data/level2.txt";
    "data/level3.txt";
    "data/level4.txt";
    "data/level5.txt";
  |]

let default_status_message = "Click a tile to place or remove a wall."

let can_go_next board =
  match reachable_from_camel board with
  | Enclosed _ -> true
  | Open -> false

let reached_max_score board =
  match reachable_from_camel board with
  | Enclosed { score; _ } -> score = board.max_score
  | Open -> false

let load_next_level level_index =
  let next_index = level_index + 1 in
  if next_index >= Array.length level_files then LevelSelect
  else
    try GameScene (Parser.load_board level_files.(next_index), next_index)
    with exn ->
      print_endline ("Could not load next level: " ^ Printexc.to_string exn);
      LevelSelect

let draw_win_popup () =
  let screen_w = get_screen_width () in
  let popup_w = 430 in
  let popup_h = 145 in
  let popup_x = (screen_w - popup_w) / 2 in
  let popup_y = 70 in

  draw_rectangle (popup_x + 6) (popup_y + 6) popup_w popup_h
    (Color.create 0 0 0 120);

  draw_rectangle popup_x popup_y popup_w popup_h (Color.create 245 235 180 245);

  draw_rectangle_lines popup_x popup_y popup_w popup_h Color.black;

  let close_size = 24 in
  let close_x = popup_x + popup_w - close_size - 10 in
  let close_y = popup_y + 10 in
  let close_rect =
    Rectangle.create (float_of_int close_x) (float_of_int close_y)
      (float_of_int close_size) (float_of_int close_size)
  in

  draw_rectangle close_x close_y close_size close_size
    (Color.create 150 60 60 255);
  draw_rectangle_lines close_x close_y close_size close_size Color.black;
  draw_text "X" (close_x + 6) (close_y + 2) 20 Color.white;

  let title = "You Win!" in
  let title_width = measure_text title 40 in
  draw_text title
    (popup_x + ((popup_w - title_width) / 2))
    (popup_y + 25) 40 Color.black;

  let msg = "Maximum enclosed space reached." in
  let msg_width = measure_text msg 20 in
  draw_text msg
    (popup_x + ((popup_w - msg_width) / 2))
    (popup_y + 88) 20 Color.black;

  close_rect

let run_game_scene assets board level_index =
  let scale = 0.15 in
  let show_win_popup = ref (reached_max_score board) in

  let layout board =
    fit_gui_tile_px_for_viewport board;
    let window_width = get_screen_width () in
    let board_width = board_pixel_width board in
    let board_offset_x = (window_width - board_width) / 2 in
    let back_button_x = board_offset_x + 10 in
    let back_button_y =
      board_pixel_height board + Render.info_panel_height + 10
    in
    let button_width =
      int_of_float (float (Texture.width assets.back) *. scale)
    in
    let button_height =
      int_of_float (float (Texture.height assets.back) *. scale)
    in
    let back_rect = button_rect back_button_x back_button_y assets.back scale in
    ( board_offset_x,
      back_button_x,
      back_button_y,
      button_width,
      button_height,
      back_rect )
  in

  let rec loop board status_message =
    if window_should_close () then Quit
    else if is_key_pressed Key.Q then Quit
    else if is_key_pressed Key.Escape then NextScene Home
    else (
      toggle_fullscreen_hotkey ();

      let ( board_offset_x,
            back_button_x,
            back_button_y,
            button_width,
            button_height,
            back_rect ) =
        layout board
      in

      let level_complete = can_go_next board in
      let max_score_win = reached_max_score board in

      let next_button_x = back_button_x + button_width + 20 in
      let next_button_y = back_button_y in
      let next_rect =
        Rectangle.create
          (float_of_int next_button_x)
          (float_of_int next_button_y)
          (float_of_int button_width)
          (float_of_int button_height)
      in

      let next_label =
        if level_index + 1 < Array.length level_files then "Next Level"
        else "Levels"
      in

      let popup_close_rect = ref None in

      begin_drawing ();
      clear_background (Color.create 110 155 80 255);

      draw_board_gui ~offset_x:board_offset_x board;
      draw_status_panel ~offset_x:board_offset_x board status_message;

      draw_rectangle back_button_x back_button_y button_width button_height
        (Color.create 110 155 80 255);
      draw_rectangle_lines back_button_x back_button_y button_width
        button_height Color.black;

      let text_width = measure_text "Home" 20 in
      draw_text "Home"
        (back_button_x + ((button_width - text_width) / 2))
        (back_button_y + ((button_height - 20) / 2))
        20 Color.white;

      if level_complete then begin
        draw_rectangle next_button_x next_button_y button_width button_height
          (Color.create 110 155 80 255);
        draw_rectangle_lines next_button_x next_button_y button_width
          button_height Color.black;

        let next_text_width = measure_text next_label 20 in
        draw_text next_label
          (next_button_x + ((button_width - next_text_width) / 2))
          (next_button_y + ((button_height - 20) / 2))
          20 Color.white
      end;

      draw_tip_section ~board_offset_x
        ~y_top:(back_button_y + button_height + 12)
        board;

      if max_score_win && !show_win_popup then
        popup_close_rect := Some (draw_win_popup ());

      end_drawing ();

      match !popup_close_rect with
      | Some rect when button_clicked rect ->
          show_win_popup := false;
          loop board status_message
      | _ ->
          if button_clicked back_rect then NextScene Home
          else if level_complete && button_clicked next_rect then
            NextScene (load_next_level level_index)
          else if is_mouse_button_pressed MouseButton.Left then
            let x = get_mouse_x () - board_offset_x in
            let y = get_mouse_y () in
            match cell_at_mouse board x y with
            | Some coord ->
                let next_board, msg =
                  placement_message board coord (place_wall board coord)
                in
                let msg =
                  match reachable_from_camel next_board with
                  | Enclosed { score; _ } ->
                      if score = next_board.max_score then
                        "Camel enclosed! Maximum enclosed space reached."
                      else
                        "Camel enclosed! Change level, or keep trying for the \
                         best score."
                  | Open -> msg
                in
                if reached_max_score next_board then show_win_popup := true;
                loop next_board msg
            | None -> loop board status_message
          else loop board status_message)
  in

  loop board default_status_message
