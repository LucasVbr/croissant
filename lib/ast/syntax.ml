(* lib/ast/syntax.ml *)

type _type =
  | IntegerType
  | FloatType
  | CharacterType
  | StringType
  | BooleanType
  | VoidType

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
  | Assign

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