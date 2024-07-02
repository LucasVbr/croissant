(* lib/ast/syntax.ml *)

type _type =
  | Type_Integer
  | Type_Float
  | Type_Character
  | Type_String
  | Type_Boolean
  | Type_Void

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

type variable_declaration =
  | VariableDeclaration of _type * expression * expression

type statement =
  | ExpressionStatement of expression
  | VariableStatement of variable_declaration list

type source_file = SourceFile of statement list