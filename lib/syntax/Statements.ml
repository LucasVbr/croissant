class virtual statement =
  object
    inherit Node.node
    method virtual check_type : Environments.typeEnvironments -> Types.t
    method virtual eval : Environments.evalEnvironment -> Literals.literal_value
  end

class sequence (left : statement) (right : statement) =
  object
    inherit statement

    method to_string =
      "Sequence(" ^ left#to_string ^ ", " ^ right#to_string ^ ")"

    method check_type env =
      let _ = left#check_type env in
      right#check_type env

    method eval env =
      let _ = left#eval env in
      right#eval env
  end

class expression (expression : Expressions.expression) =
  object
    inherit statement
    method to_string = "Expression(" ^ expression#to_string ^ ")"
    method check_type env = expression#check_type env
    method eval env = expression#eval env
  end

class variableDeclaration (name : string) (tp : Types.t)
  (expression : Expressions.expression) =
  object
    inherit statement

    method to_string =
      "VariableDeclaration(" ^ name ^ ", " ^ Types.string_of_t tp ^ ", "
      ^ expression#to_string ^ ")"

    method check_type env =
      let expression_type = expression#check_type env in
      if tp = expression_type then (
        env#add name expression_type;
        Types.VoidType)
      else raise (Failure "VariableDeclaration: type mismatch")

    method eval env =
      let value = expression#eval env in
      env#add name value;
      value
  end