type parse_error =
  | Empty_input
  | Bad_format
  | Not_an_int of string

let parse_coordinate input =
  let s = String.trim input in
  if s = "" then Error Empty_input
  else
    let len = String.length s in
    if len < 5 || s.[0] <> '[' || s.[len - 1] <> ']' then Error Bad_format
    else
      let inner = String.sub s 1 (len - 2) in
      let coords = String.split_on_char ',' inner in
      match coords with
      | [ x; y ] -> (
          let x_coord = String.trim x in
          let y_coord = String.trim y in
          if x_coord = "" || y_coord = "" then Error Bad_format
          else
            match int_of_string_opt x with
            | None -> Error (Not_an_int x)
            | Some x -> (
                match int_of_string_opt y with
                | None -> Error (Not_an_int y)
                | Some y -> Ok { Model.x; y }))
      | _ -> Error Bad_format

let parse_error_to_string = function
  | Empty_input -> "Input is empty. Enter a coordinate like [x,y]."
  | Bad_format -> "Bad coordinate format. Use [x,y]."
  | Not_an_int token ->
      "Invalid integer: \"" ^ token ^ "\". Use [x,y] with integers."
