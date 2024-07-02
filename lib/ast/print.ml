(* lib/ast/print.ml *)

open Syntax

(** [string_of_binary_operator op] returns a string representation of the binary operator [op]. *)
let string_of_binary_operator = function
  | Add -> "Add"
  | Substract -> "Substract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | AmpersandAmpersand -> "AmpersandAmpersand"
  | BarBar -> "BarBar"
  | EqualsEquals -> "EqualsEquals"
  | ExclamationEquals -> "ExclamationEquals"
  | LessThan -> "LessThan"
  | LessThanEquals -> "LessThanEquals"
  | GreaterThan -> "GreaterThan"
  | GreaterThanEquals -> "GreaterThanEquals"

(** [string_of_unary_operator op] returns a string representation of the unary operator [op]. *)
let string_of_unary_operator = function Negate -> "Negate" | Not -> "Not"

(** [string_of_literal l] returns a string representation of the literal [l]. *)
let string_of_literal = function
  | Integer i -> "Integer(" ^ string_of_int i ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Character c -> "Character('" ^ Char.escaped c ^ "')"
  | String s -> "String(\"" ^ s ^ "\")"
  | Boolean b -> "Boolean(" ^ string_of_bool b ^ ")"
  | Null -> "Null"

(** [string_of_expression e] returns a string representation of the expression [e]. *)
let rec string_of_expression = function
  | Literal l -> "Literal(" ^ string_of_literal l ^ ")"
  | Identifier i -> "Identifier(\"" ^ i ^ "\")"
  | UnaryExpression (op, e) ->
      "UnaryExpression("
      ^ string_of_unary_operator op
      ^ ", " ^ string_of_expression e ^ ")"
  | BinaryExpression (op, e1, e2) ->
      "BinaryExpression("
      ^ string_of_binary_operator op
      ^ ", " ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"

(** [string_of_statement s] returns a string representation of the statement [s]. *)
let string_of_statement = function
  | ExpressionStatement e ->
      "ExpressionStatement(" ^ string_of_expression e ^ ")"

(** [string_of_source_file f] returns a string representation of the source file [f]. *)
let string_of_source_file = function
  | SourceFile stmts ->
      let stmt_strings = List.map string_of_statement stmts in
      "SourceFile([" ^ String.concat ", " stmt_strings ^ "])"