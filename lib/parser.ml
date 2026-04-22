type parse_error =
	| Empty_input
	| Bad_format
	| Not_an_int of string

let parse_coordinate _input = failwith "TODO: Parser.parse_coordinate"

let parse_error_to_string _err = failwith "TODO: Parser.parse_error_to_string"