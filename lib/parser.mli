type parse_error =
  | Empty_input
  | Bad_format
  | Not_an_int of string

(** [parse_coordinate input] parses one user-entered coordinate string.

    Expected behavior:
    - Accept input in the form [x,y].
    - Ignore leading/trailing whitespace around the whole input and around each
      coordinate token.
    - Return [Error Empty_input] when the trimmed input is empty.
    - Return [Error Bad_format] when the input does not contain exactly one
      comma with two coordinate fields.
    - Return [Error (Not_an_int token)] when a field is not a valid integer.
    - Return [Ok {x; y}] on successful parsing.
*)
val parse_coordinate : string -> (Model.coordinate, parse_error) result

(** [parse_error_to_string err] converts parser errors into user-facing text.

    Expected behavior:
    - Produce concise, actionable messages suitable for terminal display.
    - For [Bad_format], mention the accepted format (for example [x,y]).
    - For [Not_an_int token], include the bad token to help debugging input.
*)
val parse_error_to_string : parse_error -> string
