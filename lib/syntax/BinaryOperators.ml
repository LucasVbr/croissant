class virtual binaryOperator =
  object
    inherit Node.node
    method virtual check_type : Types.t * Types.t -> Types.t

    method virtual eval
        : Literals.literal_value * Literals.literal_value ->
          Literals.literal_value
  end

class add =
  object
    inherit binaryOperator
    method to_string = "Add"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.IntegerType
      | Types.FloatType, Types.FloatType -> Types.FloatType
      | Types.FloatType, Types.IntegerType -> Types.FloatType
      | Types.IntegerType, Types.FloatType -> Types.FloatType
      | Types.StringType, Types.StringType -> Types.StringType
      | _ -> raise (Failure "Add: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Int (a + b)
      | `Float a, `Float b -> `Float (a +. b)
      | `Float a, `Int b -> `Float (a +. float_of_int b)
      | `Int a, `Float b -> `Float (float_of_int a +. b)
      | `String a, `String b -> `String (a ^ b) (* Concatenation *)
      | _ -> raise (Failure "Add: operands must be numbers")
  end

class subtract =
  object
    inherit binaryOperator
    method to_string = "Subtract"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.IntegerType
      | Types.FloatType, Types.FloatType -> Types.FloatType
      | Types.FloatType, Types.IntegerType -> Types.FloatType
      | Types.IntegerType, Types.FloatType -> Types.FloatType
      | _ -> raise (Failure "Subtract: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Int (a - b)
      | `Float a, `Float b -> `Float (a -. b)
      | `Float a, `Int b -> `Float (a -. float_of_int b)
      | `Int a, `Float b -> `Float (float_of_int a -. b)
      | _ -> raise (Failure "Subtract: operands must be numbers")
  end

class multiply =
  object
    inherit binaryOperator
    method to_string = "Multiply"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.FloatType
      | Types.FloatType, Types.FloatType -> Types.FloatType
      | Types.FloatType, Types.IntegerType -> Types.FloatType
      | Types.IntegerType, Types.FloatType -> Types.FloatType
      | _ -> raise (Failure "Multiply: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Float (float_of_int a *. float_of_int b)
      | `Float a, `Float b -> `Float (a *. b)
      | `Float a, `Int b -> `Float (a *. float_of_int b)
      | `Int a, `Float b -> `Float (float_of_int a *. b)
      | _ -> raise (Failure "Multiply: operands must be numbers")
  end

class divide =
  object
    inherit binaryOperator
    method to_string = "Divide"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.FloatType
      | Types.FloatType, Types.FloatType -> Types.FloatType
      | Types.FloatType, Types.IntegerType -> Types.FloatType
      | Types.IntegerType, Types.FloatType -> Types.FloatType
      | _ -> raise (Failure "Divide: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Float (float_of_int a /. float_of_int b)
      | `Float a, `Float b -> `Float (a /. b)
      | `Float a, `Int b -> `Float (a /. float_of_int b)
      | `Int a, `Float b -> `Float (float_of_int a /. b)
      | _ -> raise (Failure "Divide: operands must be numbers")
  end

class equals =
  object
    inherit binaryOperator
    method to_string = "Equals"

    method check_type (left, right) =
      if left = right then Types.BooleanType
      else raise (Failure "Equals: operands must have the same type")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a = b)
      | `Float a, `Float b -> `Bool (a = b)
      | `Float a, `Int b -> `Bool (a = float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a = b)
      | `String a, `String b -> `Bool (a = b)
      | `Bool a, `Bool b -> `Bool (a = b)
      | `Char a, `Char b -> `Bool (a = b)
      | `Null, `Null -> `Bool true
      | `Void, `Void -> `Bool true
      | _ -> raise (Failure "Equals: operands must have the same type")
  end

class notEquals =
  object
    inherit binaryOperator
    method to_string = "NotEquals"

    method check_type (left, right) =
      if left = right then Types.BooleanType
      else raise (Failure "Equals: operands must have the same type")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a <> b)
      | `Float a, `Float b -> `Bool (a <> b)
      | `Float a, `Int b -> `Bool (a <> float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a <> b)
      | `String a, `String b -> `Bool (a <> b)
      | `Bool a, `Bool b -> `Bool (a <> b)
      | `Char a, `Char b -> `Bool (a <> b)
      | `Null, `Null -> `Bool false
      | `Void, `Void -> `Bool false
      | _ -> raise (Failure "NotEquals: operands must have the same type")
  end

class lessThan =
  object
    inherit binaryOperator
    method to_string = "LessThan"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.BooleanType
      | Types.FloatType, Types.FloatType -> Types.BooleanType
      | Types.FloatType, Types.IntegerType -> Types.BooleanType
      | Types.IntegerType, Types.FloatType -> Types.BooleanType
      | _ -> raise (Failure "LessThan: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a < b)
      | `Float a, `Float b -> `Bool (a < b)
      | `Float a, `Int b -> `Bool (a < float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a < b)
      | _ -> raise (Failure "LessThan: operands must be numbers")
  end

class lessThanEquals =
  object
    inherit binaryOperator
    method to_string = "LessThanEquals"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.BooleanType
      | Types.FloatType, Types.FloatType -> Types.BooleanType
      | Types.FloatType, Types.IntegerType -> Types.BooleanType
      | Types.IntegerType, Types.FloatType -> Types.BooleanType
      | _ -> raise (Failure "LessThanEquals: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a <= b)
      | `Float a, `Float b -> `Bool (a <= b)
      | `Float a, `Int b -> `Bool (a <= float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a <= b)
      | _ -> raise (Failure "LessThanEquals: operands must be numbers")
  end

class greaterThan =
  object
    inherit binaryOperator
    method to_string = "GreaterThan"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.BooleanType
      | Types.FloatType, Types.FloatType -> Types.BooleanType
      | Types.FloatType, Types.IntegerType -> Types.BooleanType
      | Types.IntegerType, Types.FloatType -> Types.BooleanType
      | _ -> raise (Failure "GreaterThan: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a > b)
      | `Float a, `Float b -> `Bool (a > b)
      | `Float a, `Int b -> `Bool (a > float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a > b)
      | _ -> raise (Failure "GreaterThan: operands must be numbers")
  end

class greaterThanEquals =
  object
    inherit binaryOperator
    method to_string = "GreaterThanEquals"

    method check_type (left, right) =
      match (left, right) with
      | Types.IntegerType, Types.IntegerType -> Types.BooleanType
      | Types.FloatType, Types.FloatType -> Types.BooleanType
      | Types.FloatType, Types.IntegerType -> Types.BooleanType
      | Types.IntegerType, Types.FloatType -> Types.BooleanType
      | _ -> raise (Failure "GreaterThanEquals: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Int a, `Int b -> `Bool (a >= b)
      | `Float a, `Float b -> `Bool (a >= b)
      | `Float a, `Int b -> `Bool (a >= float_of_int b)
      | `Int a, `Float b -> `Bool (float_of_int a >= b)
      | _ -> raise (Failure "GreaterThanEquals: operands must be numbers")
  end

class logicalAnd =
  object
    inherit binaryOperator
    method to_string = "And"

    method check_type (left, right) =
      match (left, right) with
      | Types.BooleanType, Types.BooleanType -> Types.BooleanType
      | _ -> raise (Failure "And: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Bool a, `Bool b -> `Bool (a && b)
      | _ -> raise (Failure "And: operands must be booleans")
  end

class logicalOr =
  object
    inherit binaryOperator
    method to_string = "Or"

    method check_type (left, right) =
      match (left, right) with
      | Types.BooleanType, Types.BooleanType -> Types.BooleanType
      | _ -> raise (Failure "Or: operands must be numbers")

    method eval (left, right) =
      match (left, right) with
      | `Bool a, `Bool b -> `Bool (a || b)
      | _ -> raise (Failure "Or: operands must be booleans")
  end