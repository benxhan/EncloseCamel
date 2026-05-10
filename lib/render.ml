open Model
open Raylib

let base_gui_tile_px = 64
let tile_size = base_gui_tile_px

let gui_tile_px_ref = ref base_gui_tile_px

let gui_tile_px () = !gui_tile_px_ref

let set_gui_tile_px n =
  gui_tile_px_ref := max 1 (min base_gui_tile_px n)

let info_panel_height = 96
let asset_dir = "gui/assets"

type texture_assets = {
  camel : Texture.t array;
  water : Texture.t array;
  wall : Texture.t array;
  blank : Texture.t array;
  enclosed_blank : Texture.t array;
  corn_camel : Texture.t array;
  cherry : Texture.t array;
  bees : Texture.t array;
  golden_apple : Texture.t array;
  lava_bucket : Texture.t array;
  empty_bucket : Texture.t array;
  portal : Texture.t array;
}

let textures : texture_assets option ref = ref None
let texture_path file_name = Filename.concat asset_dir file_name

let load_texture_frames base_name =
  let rec collect index acc =
    let file_name = Printf.sprintf "%s%d.png" base_name index in
    let path = texture_path file_name in
    if Sys.file_exists path then collect (index + 1) (load_texture path :: acc)
    else acc
  in
  let frames = List.rev (collect 1 []) in
  match frames with
  | [] ->
      let file_name = base_name ^ ".png" in
      let path = texture_path file_name in
      if Sys.file_exists path then [| load_texture path |]
      else failwith ("Missing raylib asset sequence for: " ^ base_name)
  | _ -> Array.of_list frames

let load_gui_textures () =
  match !textures with
  | Some assets -> assets
  | None ->
      let assets =
        {
          camel = load_texture_frames "camel";
          water = load_texture_frames "water";
          wall = load_texture_frames "wall";
          blank = load_texture_frames "blank";
          enclosed_blank = load_texture_frames "corn";
          corn_camel = load_texture_frames "cornCamel";
          cherry = load_texture_frames "cherry";
          bees = load_texture_frames "bees";
          golden_apple = load_texture_frames "golden_apple";
          lava_bucket = load_texture_frames "lava_bucket";
          empty_bucket = load_texture_frames "empty_bucket";
          portal = load_texture_frames "portal";
        }
      in
      textures := Some assets;
      assets

let init_gui_textures () = ignore (load_gui_textures ())

let unload_gui_textures () =
  match !textures with
  | None -> ()
  | Some assets ->
      Array.iter unload_texture assets.camel;
      Array.iter unload_texture assets.water;
      Array.iter unload_texture assets.wall;
      Array.iter unload_texture assets.blank;
      Array.iter unload_texture assets.enclosed_blank;
      Array.iter unload_texture assets.corn_camel;
      Array.iter unload_texture assets.cherry;
      Array.iter unload_texture assets.bees;
      Array.iter unload_texture assets.golden_apple;
      Array.iter unload_texture assets.lava_bucket;
      Array.iter unload_texture assets.empty_bucket;
      Array.iter unload_texture assets.portal;
      textures := None

let current_frame frames =
  let seconds = get_time () in
  let frame_count = Array.length frames in
  if frame_count = 0 then failwith "No texture frames loaded"
  else frames.(int_of_float (floor seconds) mod frame_count)

let portal_color id =
  let r = (id * 50 + 100) mod 256 in
  let g = (id * 100 + 50) mod 256 in
  let b = (id * 150 + 200) mod 256 in
  Color.create r g b 255

let draw_tile_texture ?(color = Color.white) ?(offset_x = 0) ?(offset_y = 0) texture r c =
  let ts = gui_tile_px () in
  let x = c * ts + offset_x in
  let y = r * ts + offset_y in
  let scale = float ts /. float (Texture.width texture) in
  draw_texture_ex texture (Vector2.create (float x) (float y)) 0.0 scale color;
  draw_rectangle_lines x y ts ts (Color.create 255 255 255 20)

let draw_board_gui ?(offset_x = 0) ?(offset_y = 0) board =
  let { camel; water; wall; blank; enclosed_blank; corn_camel; cherry; bees; golden_apple; lava_bucket; empty_bucket; portal } = load_gui_textures () in
  let win_state = reachable_from_camel board in
  let camel_tex = current_frame camel in
  let water_tex = current_frame water in
  let wall_tex = current_frame wall in
  let blank_tex = current_frame blank in
  let enclosed_blank_tex = current_frame enclosed_blank in
  let corn_camel_tex = current_frame corn_camel in
  let cherry_tex = current_frame cherry in
  let bees_tex = current_frame bees in
  let golden_apple_tex = current_frame golden_apple in
  let lava_bucket_tex = current_frame lava_bucket in
  let empty_bucket_tex = current_frame empty_bucket in
  let portal_tex = current_frame portal in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c tile ->
          match tile with
          | Camel ->
              let texture =
                match win_state with
                | Open -> camel_tex
                | Enclosed _ -> corn_camel_tex
              in
              draw_tile_texture ~offset_x ~offset_y texture r c
          | Water -> draw_tile_texture ~offset_x ~offset_y water_tex r c
          | Wall -> draw_tile_texture ~offset_x ~offset_y wall_tex r c
          | Blank ->
              let texture =
                match win_state with
                | Open -> blank_tex
                | Enclosed { tiles; _ } ->
                    if tiles.(r).(c) then enclosed_blank_tex else blank_tex
              in
              draw_tile_texture ~offset_x ~offset_y texture r c
          | Cherry -> draw_tile_texture ~offset_x ~offset_y cherry_tex r c
          | Bees -> draw_tile_texture ~offset_x ~offset_y bees_tex r c
          | GoldenApple -> draw_tile_texture ~offset_x ~offset_y golden_apple_tex r c
          | LavaBucket ->
              let texture =
                match win_state with
                | Open -> lava_bucket_tex
                | Enclosed { tiles; _ } ->
                    if tiles.(r).(c) then empty_bucket_tex else lava_bucket_tex
              in
              draw_tile_texture ~offset_x ~offset_y texture r c
          | Portal id -> draw_tile_texture ~offset_x ~offset_y ~color:(portal_color id) portal_tex r c)
        row)
    board.grid;
  let rows = Array.length board.grid in
  let cols = Array.length board.grid.(0) in
  let ts = gui_tile_px () in
  draw_rectangle_lines offset_x offset_y (cols * ts) (rows * ts) Color.black

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
          | Portal id ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_magenta ]
                (string_of_int id)
          | Cherry | Bees | GoldenApple | LavaBucket ->
              let props = properties_of tile in
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_magenta ]
                (String.make 1 props.file_char)
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
    | Portal id -> string_of_int id ^ " "
    | (Cherry | Bees | GoldenApple | LavaBucket) as t ->
        let props = properties_of t in
        String.make 1 props.file_char ^ " "
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
