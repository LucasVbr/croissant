type types =
  | IntegerType
  | FloatType
  | CharacterType
  | StringType
  | BooleanType
  | VoidType

let pp_types = function
  | IntegerType -> "IntegerType"
  | FloatType -> "FloatType"
  | CharacterType -> "CharacterType"
  | StringType -> "StringType"
  | BooleanType -> "BooleanType"
  | VoidType -> "VoidType"
