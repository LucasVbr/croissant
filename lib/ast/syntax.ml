(* lib/ast/syntax.ml *)

type binary_operator = Add | Substract | Multiply | Divide
type unary_operator = Negate

type expression =
  | IntegerLiteral of int
  | UnaryExpression of unary_operator * expression
  | BinaryExpression of binary_operator * expression * expression

type statement = ExpressionStatement of expression
type source_file = SourceFile of statement list
