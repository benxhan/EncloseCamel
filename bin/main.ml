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
    | Ok coord ->
        (* Attempt to place a rock and branch on the result. *)
        (match place_wall board coord with
        | Ok next_board ->
            (* Successful move: confirm and continue with updated state. *)
            print_endline "Placed rock.";
            loop next_board
        | Error bad_move ->
            (* Failed move: explain why and continue with current state. *)
            print_place_result bad_move;
            loop board)

let () =
  (* Create the initial fixed-size board for the first playable version. *)
  let board = init ~width:10 ~height:8 ~camel:{r=0;c=0} ~water:[] ~walls_available:10 in
  (* Display startup guidance before entering the interactive loop. *)
  print_endline "ASCII Rock Map";
  print_endline "Coordinates are zero-based. Example: 3,4";
  (* Hand control to the recursive game loop. *)
  loop board
