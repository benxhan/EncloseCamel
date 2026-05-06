open Model
open Raylib

let tile_size = 64
let info_panel_height = 96
let asset_dir = "gui/assets"

type texture_assets = {
  camel : Texture.t;
  water : Texture.t;
  wall : Texture.t;
  blank : Texture.t;
}

let textures : texture_assets option ref = ref None
let texture_path file_name = Filename.concat asset_dir file_name

let load_gui_textures () =
  match !textures with
  | Some assets -> assets
  | None ->
      let load name =
        let path = texture_path name in
        if not (Sys.file_exists path) then
          failwith ("Missing raylib asset: " ^ path)
        else load_texture path
      in
      let assets =
        {
          camel = load "camel.png";
          water = load "water.png";
          wall = load "wall.png";
          blank = load "blank.png";
        }
      in
      textures := Some assets;
      assets

let init_gui_textures () = ignore (load_gui_textures ())

let unload_gui_textures () =
  match !textures with
  | None -> ()
  | Some assets ->
      unload_texture assets.camel;
      unload_texture assets.water;
      unload_texture assets.wall;
      unload_texture assets.blank;
      textures := None

let draw_tile_texture texture r c =
  let x = c * tile_size in
  let y = r * tile_size in
  draw_texture texture x y Color.white;
  draw_rectangle_lines x y tile_size tile_size Color.black

let blank_tile_overlay win_state row col =
  match win_state with
  | Open -> Color.create 140 212 124 140
  | Enclosed { tiles; _ } ->
      if tiles.(row).(col) then Color.create 255 238 88 150
      else Color.create 140 212 124 120

let draw_board_gui board =
  let { camel; water; wall; blank } = load_gui_textures () in
  let win_state = reachable_from_camel board in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c tile ->
          match tile with
          | Camel -> draw_tile_texture camel r c
          | Water -> draw_tile_texture water r c
          | Wall -> draw_tile_texture wall r c
          | Blank ->
              draw_tile_texture blank r c;
              draw_rectangle (c * tile_size) (r * tile_size) tile_size tile_size
                (blank_tile_overlay win_state r c)
          | Logs -> draw_tile_texture blank r c)
        row)
    board.grid;
  let rows = Array.length board.grid in
  let cols = Array.length board.grid.(0) in
  draw_rectangle_lines 0 0 (cols * tile_size) (rows * tile_size) Color.black

let render_board _board =
  let win_state = reachable_from_camel _board in
  let () =
    Array.iteri
      (fun r_ind row ->
        let tileprint c_ind tile =
          match tile with
          | Camel ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_red ]
                "C"
          (* print_string " " *)
          | Water ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_blue ]
                "W"
          (* print_string " " *)
          | Wall ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_black ]
                "B"
          | Logs ->
              ANSITerminal.print_string
                [ ANSITerminal.red; ANSITerminal.on_black ]
                "L"
          (* print_string " " *)
          | Blank -> (
              match win_state with
              | Open ->
                  ANSITerminal.print_string
                    [ ANSITerminal.white; ANSITerminal.on_green ]
                    "G"
              (* print_string " " *)
              | Enclosed area -> (
                  let check_tile = area.tiles.(r_ind).(c_ind) in
                  match check_tile with
                  | true ->
                      ANSITerminal.print_string
                        [ ANSITerminal.black; ANSITerminal.on_yellow ]
                        "G"
                  (* print_string " " *)
                  | false ->
                      ANSITerminal.print_string
                        [ ANSITerminal.white; ANSITerminal.on_green ]
                        "G"))
          (* print_string " " *)
        in
        Array.iteri tileprint row;
        print_newline ())
      _board.grid
  in
  let () =
    print_endline
      ("Coordinates of the Camel are ("
      ^ string_of_int _board.camel_loc.r
      ^ ", "
      ^ string_of_int _board.camel_loc.c
      ^ ")")
  in
  print_endline
    ("Number of walls remaining: "
    ^ string_of_int (_board.walls_remaining + !(_board.bonus_walls)))

let str_render_board board =
  let buf = Buffer.create 128 in

  let tile_to_string = function
    | Camel -> "C "
    | Water -> "W "
    | Wall -> "B "
    | Blank -> "G "
    | Logs -> "L "
  in

  Array.iter
    (fun row ->
      Array.iter (fun tile -> Buffer.add_string buf (tile_to_string tile)) row;
      Buffer.add_char buf '\n')
    board.grid;

  Buffer.add_string buf "Coordinates of the Camel are (_, _)\n";
  Buffer.add_string buf
    ("Number of walls remaining: " ^ string_of_int board.walls_remaining);

  Buffer.contents buf

let print_place_result _result =
  match _result with
  | Out_of_bounds -> print_endline "Coordinates out of bounds! Try again"
  | Occupied -> print_endline "This spot is occupied! Try again"
  | Ok -> ()
