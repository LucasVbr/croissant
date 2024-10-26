type t =
  | IntegerType
  | FloatType
  | BooleanType
  | StringType
  | CharacterType
  | NullType
  | VoidType

let string_of_t = function
  | IntegerType -> "IntegerType"
  | FloatType -> "FloatType"
  | BooleanType -> "BooleanType"
  | StringType -> "StringType"
  | CharacterType -> "CharacterType"
  | NullType -> "NullType"
  | VoidType -> "VoidType"