type binary_operators =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equals
  | NotEquals
  | LessThan
  | LessThanEquals
  | GreaterThan
  | GreaterThanEquals
  | And
  | Or

let pp_binary_operators = function
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Equals -> "Equals"
  | NotEquals -> "NotEquals"
  | LessThan -> "LessThan"
  | LessThanEquals -> "LessThanEquals"
  | GreaterThan -> "GreaterThan"
  | GreaterThanEquals -> "GreaterThanEquals"
  | And -> "And"
  | Or -> "Or"