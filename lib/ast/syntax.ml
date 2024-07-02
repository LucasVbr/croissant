(* lib/ast/syntax.ml *)

type literal =
  | Integer of int
  | Float of float
  | Character of char
  | String of string
  | Boolean of bool
  | Null

type binary_operator =
  | Add
  | Substract
  | Multiply
  | Divide
  | AmpersandAmpersand
  | BarBar
  | EqualsEquals
  | ExclamationEquals
  | LessThan
  | LessThanEquals
  | GreaterThan
  | GreaterThanEquals

type unary_operator = Negate | Not

type expression =
  | Literal of literal
  | Identifier of string
  | UnaryExpression of unary_operator * expression
  | BinaryExpression of binary_operator * expression * expression

type statement = ExpressionStatement of expression
type source_file = SourceFile of statement list