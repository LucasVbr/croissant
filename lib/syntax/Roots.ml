class program (statement : Statements.statement) =
  object
    inherit Node.node
    method to_string = "Program(" ^ statement#to_string ^ ")"

    method check_type =
      let env : Environments.typeEnvironments =
        new Environments.typeEnvironments
      in
      statement#check_type env

    method eval =
      let env : Environments.evalEnvironment =
        new Environments.evalEnvironment
      in
      statement#eval env
  end