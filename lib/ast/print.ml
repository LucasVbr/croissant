(* lib/ast/print.ml *)

open Syntax

(** [string_of_type t] returns a string representation of the type [t]. *)
let string_of_type = function
  | IntegerType -> "IntegerType"
  | FloatType -> "FloatType"
  | CharacterType -> "CharacterType"
  | StringType -> "StringType"
  | BooleanType -> "BooleanType"
  | VoidType -> "VoidType"

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
  | Assign -> "Assign"

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

let string_of_variable_declaration = function
  | VariableDeclaration (t, id, e) ->
      "VariableDeclaration(" ^ string_of_type t ^ ", " ^ string_of_expression id
      ^ ", " ^ string_of_expression e ^ ")"

(** [string_of_statement s] returns a string representation of the statement [s]. *)
let string_of_statement = function
  | ExpressionStatement e ->
      "ExpressionStatement(" ^ string_of_expression e ^ ")"
  | VariableStatement decls ->
      let decl_strings = List.map string_of_variable_declaration decls in
      "VariableStatement([" ^ String.concat ", " decl_strings ^ "])"

(** [string_of_source_file f] returns a string representation of the source file [f]. *)
let string_of_source_file = function
  | SourceFile stmts ->
      let stmt_strings = List.map string_of_statement stmts in
      "SourceFile([" ^ String.concat ", " stmt_strings ^ "])"

(** The signature of the module [Print]. *)
module type Print = sig
  val string_of_type : _type -> string
  val string_of_binary_operator : binary_operator -> string
  val string_of_unary_operator : unary_operator -> string
  val string_of_literal : literal -> string
  val string_of_expression : expression -> string
  val string_of_variable_declaration : variable_declaration -> string
  val string_of_statement : statement -> string
  val string_of_source_file : source_file -> string
end