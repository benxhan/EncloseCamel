open EncloseCamel
open Model
open Parser
open Render

let rec loop board =
  (* Draw the current board at the start of each turn. *)
  render_board board;
  (* Ask the player for either a coordinate or the quit command. *)
  print_string "\nEnter coordinate as x,y (or 'quit'): ";
  (* Read exactly one line of terminal input for this turn. *)
  let input = read_line () in
  (* End the loop if the player asked to quit (case-insensitive). *)
  if String.lowercase_ascii (String.trim input) = "quit" then
    print_endline "Goodbye!"
  else
    (* Parse the input into a coordinate before touching game state. *)
    match parse_coordinate input with
    | Error err ->
        (* Report parse errors and keep the same board state. *)
        print_endline (parse_error_to_string err);
        loop board
    | Ok coord -> (
        (* Attempt to place a rock and branch on the result. *)
        match place_wall board coord with
        | Ok next_board ->
            let state = reachable_from_camel next_board in
            (match state with
            | Open -> 
                print_endline "Placed rock.";
                loop next_board
            | Enclosed { score; _ } ->
                if score = next_board.max_score then begin
                  print_endline ("You won! Max score of " ^ string_of_int score ^ " achieved.");
                  loop next_board
                end else begin
                  print_endline ("Score: " ^ string_of_int score);
                  loop next_board
                end)
        | Error bad_move ->
            (* Failed move: explain why and continue with current state. *)
            print_place_result bad_move;
            loop board)

let default_level_file = "data/basicworld.txt"

(* [write_default_level_file ()] overwrites [default_level_file] with the
   original default board layout. Used when [data/basicworld.txt] is missing or
   cannot be parsed. *)
let write_default_level_file () =
  let original_file = open_out default_level_file in
  output_string original_file
    "2\n\
     C G G G G G G G G G\n\
     G G G G G G G G W G\n\
     G G G G G G G W G G\n\
     G G G G G G W G G G\n\
     G G G G G W G G G G\n\
     G G G G W G G G G G\n\
     G G G W G G G G G G\n\
     G G W G G G G G G G\n\
     G W G G G G G G G G\n\
     G G G G G G G G G G\n";
  close_out original_file

(* [load_default_board ()] returns the default board, or calls
   [write_default_level_file ()] if no default board exists*)
let load_default_board () =
  if not (Sys.file_exists default_level_file) then (
    print_endline "Default board file not found. Creating default board.";
    write_default_level_file ());

  try Parser.load_board default_level_file
  with exn ->
    print_endline
      ("Default board is invalid. Recreating board: " ^ Printexc.to_string exn);
    write_default_level_file ();
    load_board default_level_file

(* [load_starting_board ()] returns the requested board or falls back to the
   default board when loading fails. *)
let load_starting_board () =
  match Array.to_list Sys.argv with
  | [ program_name ] ->
      print_endline "No level file provided. Using default board";
      print_endline
        "To input a board, please use: dune exec bin/main.exe -- <level-file>";
      load_default_board ()
  | [ program_name; filename ] -> (
      try
        print_endline ("Loading level file: " ^ filename);
        Parser.load_board filename
      with
      | Sys_error msg ->
          print_endline ("Could not load level file: " ^ msg);
          print_endline "Using default board instead";
          load_default_board ()
      | Failure msg ->
          print_endline ("Error reading file: " ^ msg);
          print_endline "Using default board instead";
          load_default_board ()
      | exn ->
          print_endline
            ("Unexpected error while reading file: " ^ Printexc.to_string exn);
          print_endline "Using default board instead";
          load_default_board ())
  | _ ->
      print_endline "Unexpected Input. Using default board instead.";
      print_endline
        "To input a board, please use: dune exec bin/main.exe -- <level-file>";
      load_default_board ()

let () =
  (* New title page lol *)
  print_string 
  {|
  ___________             .__                      _________                       .__   
\_   _____/ ____   ____ |  |   ____  ______ ____ \_   ___ \_____    _____   ____ |  |  
 |    __)_ /    \_/ ___\|  |  /  _ \/  ___// __ \/    \  \/\__  \  /     \_/ __ \|  |  
 |        \   |  \  \___|  |_(  <_> )___ \\  ___/\     \____/ __ \|  Y Y  \  ___/|  |__
/_______  /___|  /\___  >____/\____/____  >\___  >\______  (____  /__|_|  /\___  >____/
        \/     \/     \/                \/     \/        \/     \/      \/     \/      
  |};
  (* Create the initial fixed-size board for the first playable version. *)
  let board = load_starting_board () in
  (* Display startup guidance before entering the interactive loop. *)
  print_endline "ASCII Rock Map";
  print_endline "Coordinates are zero-based. Example: 3,4";
  (* Hand control to the recursive game loop. *)
  loop board
