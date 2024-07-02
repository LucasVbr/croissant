(* lib/ast.ml *)

type binary_operator = Add | Substract | Multiply | Divide

type expression =
  | IntegerLiteral of int
  | BinaryExpression of binary_operator * expression * expression

type statement = ExpressionStatement of expression
type source_file = SourceFile of statement list

(* Print AST *)

let string_of_binary_operator = function
  | Add -> "Add"
  | Substract -> "Substract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"

let rec string_of_expression = function
  | IntegerLiteral i -> "IntegerLiteral(" ^ string_of_int i ^ ")"
  | BinaryExpression (op, e1, e2) ->
      "BinaryExpression("
      ^ string_of_binary_operator op
      ^ ", " ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"

let string_of_statement = function
  | ExpressionStatement e ->
      "ExpressionStatement(" ^ string_of_expression e ^ ")"

let string_of_source_file = function
  | SourceFile stmts ->
      let stmt_strings = List.map string_of_statement stmts in
      "SourceFile([" ^ String.concat ", " stmt_strings ^ "])"
