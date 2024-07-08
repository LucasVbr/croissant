type binary_operators = Add | Subtract | Multiply | Divide

let pp_binary_operators = function
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
