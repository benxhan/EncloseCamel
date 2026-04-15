

let () = print_endline "Hello, World!"

let rec prompt_and_print () =
  let () =
    print_string
      "Enter dictionary filename in data directory to play Wordle or \"quit\" \
       to exit > "
  in
  let the_input = read_line () in
  match the_input with
  | exception Failure s -> failwith "Incorrect file path"
  | _ ->
      let () =
        print_endline
          "Enable cheat mode to see the answer? enter y for yes or anyhing \
           else to cancel > "
      in
      let cheat_input = read_line () in
      if the_input = "quit" then () else prompt_and_print ()

let () = prompt_and_print ()
