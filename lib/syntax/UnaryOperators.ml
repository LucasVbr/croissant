class virtual unaryOperator =
  object
    inherit Node.node
    method virtual check_type : Types.t -> Types.t
    method virtual eval : Literals.literal_value -> Literals.literal_value
  end

class arithmeticNegation =
  object
    inherit unaryOperator
    method to_string = "ArithmeticNegation"

    method check_type (operand : Types.t) =
      match operand with
      | Types.IntegerType -> Types.IntegerType
      | Types.FloatType -> Types.FloatType
      | _ -> raise (Failure "ArithmeticNegation: operand must be a number")

    method eval operand =
      match operand with
      | `Int i -> `Int (-i)
      | `Float f -> `Float (-.f)
      | _ -> raise (Failure "ArithmeticNegation: operand must be a number")
  end

class logicalNegation =
  object
    inherit unaryOperator
    method to_string = "LogicalNegation"

    method check_type (operand : Types.t) =
      match operand with
      | Types.BooleanType -> Types.BooleanType
      | _ -> raise (Failure "LogicalNegation: operand must be a boolean")

    method eval operand =
      match operand with
      | `Bool b -> `Bool (not b)
      | _ -> raise (Failure "LogicalNegation: operand must be a boolean")
  end