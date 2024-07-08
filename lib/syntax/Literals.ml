type literals =
  | Integer of int
  | Float of float
  | Character of char
  | String of string
  | Boolean of bool
  | Null

let pp_literals = function
  | Integer i -> Printf.sprintf "Integer(%d)" i
  | Float f -> Printf.sprintf "Float(%f)" f
  | Character c -> Printf.sprintf "Character('%c')" c
  | String s -> Printf.sprintf "String(\"%s\")" s
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Null -> "Null"