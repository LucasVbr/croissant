class virtual expression =
  object
    inherit Node.node
    method virtual check_type : Environments.typeEnvironments -> Types.t
    method virtual eval : Environments.evalEnvironment -> Literals.literal_value
  end

class literal (lit : Literals.literal) =
  object
    inherit expression
    method to_string = "Literal(" ^ lit#to_string ^ ")"
    method check_type _ = lit#check_type
    method eval _ = lit#value
  end

class identifier (name : string) =
  object
    inherit expression
    method to_string = "Identifier(" ^ name ^ ")"

    method check_type env =
      try env#find name
      with Not_found ->
        raise (Failure ("Identifier '" ^ name ^ "' not found"))

    method eval env = env#find name
  end

class unaryOperation (operator : UnaryOperators.unaryOperator)
  (operand : expression) =
  object
    inherit expression

    method to_string =
      "UnaryOperation(" ^ operator#to_string ^ ", " ^ operand#to_string ^ ")"

    method check_type env =
      let operand_type = operand#check_type env in
      operator#check_type operand_type

    method eval env = operator#eval (operand#eval env)
  end

class binaryOperation (operator : BinaryOperators.binaryOperator)
  (left : expression) (right : expression) =
  object
    inherit expression

    method to_string =
      "BinaryOperation(" ^ operator#to_string ^ ", " ^ left#to_string ^ ", "
      ^ right#to_string ^ ")"

    method check_type env =
      let left_type = left#check_type env in
      let right_type = right#check_type env in
      operator#check_type (left_type, right_type)

    method eval env = operator#eval (left#eval env, right#eval env)
  end