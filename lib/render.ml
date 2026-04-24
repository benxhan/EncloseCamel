let render_board _board =
  let () =
    Array.iter
      (fun row ->
        let tileprint = function
          | Camel ->
              Printf.printf "%d "
                (ANSITerminal.print_string
                   [ ANSITerminal.red; ANSITerminal.on_black ]
                   "C")
          | Water ->
              Printf.printf "%d "
                (ANSITerminal.print_string
                   [ ANSITerminal.blue; ANSITerminal.on_black ]
                   "W")
          | Wall ->
              Printf.printf "%d "
                (ANSITerminal.print_string
                   [ ANSITerminal.gray; ANSITerminal.on_black ]
                   "B")
          | Blank ->
              Printf.printf "%d "
                (ANSITerminal.print_string
                   [ ANSITerminal.green; ANSITerminal.on_black ]
                   "G")
        in
        Array.iter tileprint row;
        print_newline ())
      _board.grid
  in
  let () = print_endline "Coordinates of the Camel are (_, _)" in
  print_endline
    ("Number of walls remaining: " ^ string_of_int _board.walls_remaining)

let print_place_result _result =
  match _result with
  | Out_of_bounds -> print_endline "Coordinates out of bounds! Try again"
  | Occupied -> print_endline "This spot is occupied! Try again"
  | Ok _ -> ()
