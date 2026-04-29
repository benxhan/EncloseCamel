open Model

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
                "C";
              (* print_string " " *)
          | Water ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_blue ]
                "W";
              (* print_string " " *)
          | Wall ->
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_black ]
                "B";
              (* print_string " " *)
          | Blank -> match win_state with
            | Open -> 
              ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_green ]
                "G";
              (* print_string " " *)
            | Enclosed area -> 
              let check_tile = area.tiles.(r_ind).(c_ind) in 
              match check_tile with 
              | true -> 
              ANSITerminal.print_string
                [ ANSITerminal.black; ANSITerminal.on_yellow ]
                "G";
                (* print_string " " *)
              | false -> ANSITerminal.print_string
                [ ANSITerminal.white; ANSITerminal.on_green ]
                "G";
                (* print_string " " *)
        in
        Array.iteri tileprint row;
        print_newline ())
      _board.grid 
  in
  let () = print_endline ("Coordinates of the Camel are ("^(string_of_int _board.camel_loc.r)^", "^(string_of_int _board.camel_loc.c)^")") in
  print_endline
    ("Number of walls remaining: " ^ string_of_int _board.walls_remaining)

let str_render_board board =
  let buf = Buffer.create 128 in

  let tile_to_string = function
    | Camel -> "C "
    | Water -> "W "
    | Wall -> "B "
    | Blank -> "G "
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
