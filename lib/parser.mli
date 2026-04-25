type parse_error =
  | Empty_input
  | Bad_format
  | Not_an_int of string

(** [parse_coordinate input] parses one user-entered coordinate string.

    Expected behavior:
    - Accept input in the form [r,c].
    - Ignore leading/trailing whitespace around the whole input and around each
      coordinate token.
    - Return [Error Empty_input] when the trimmed input is empty.
    - Return [Error Bad_format] when the input does not contain exactly one
      comma with two coordinate fields.
    - Return [Error (Not_an_int token)] when a field is not a valid integer.
    - Return [Ok {r; c}] on successful parsing. *)
val parse_coordinate : string -> (Model.coordinate, parse_error) result

(** [parse_error_to_string err] converts parser errors into user-facing text.

    Expected behavior:
    - Produce concise, actionable messages suitable for terminal display.
    - For [Bad_format], mention the accepted format (for example [r,c]).
    - For [Not_an_int token], include the bad token to help debugging input. *)
val parse_error_to_string : parse_error -> string

(** [load_board filename] reads a character grid from a text file and
    reconstructs a [Model.board] value. Expected behavior:
    - Open and read [filename], splitting contents into lines.
    - Ignore spaces within each line; map non-space characters to tiles: ['C'] →
      [Camel], ['W'] → [Water], ['B'] → [Wall], ['G'] → [Blank].
    - Treat each line as one row and build a [tile array array] for
      [board.grid].
    - Infer [board.camel] from the position of the single ['C'] character.
    - Infer [board.walls_remaining] by reading the first line of the file.
    - Infer [board.max_score] by reading the second line of the file.
    - Raise [Failure] with a descriptive message on any of the following:
    - The file does not exist or cannot be opened.
    - The file is empty (zero non-empty lines).
    - The grid is non-rectangular (rows differ in tile count).
    - An unrecognised character is encountered.
    - No camel tile is found in the grid.
    - More than one camel tile is found in the grid. *)
val load_board : string -> Model.board
