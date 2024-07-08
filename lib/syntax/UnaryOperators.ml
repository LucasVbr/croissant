type unary_operators = ArithmeticNegation | LogicalNegation

let pp_unary_operators = function
  | ArithmeticNegation -> "ArithmeticNegation"
  | LogicalNegation -> "LogicalNegation"
