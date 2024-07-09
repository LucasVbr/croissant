open Alcotest
open Syntax

let test_pp_expressions () =
  let to_check =
    [
      ( "Should return \"Literal(...)\"",
        "Literal(Integer(1))",
        Expressions.Literal (Literals.Integer 1) );
      ( "Should return \"Identifier(...)\"",
        "Identifier(\"myVar\")",
        Expressions.Identifier "myVar" );
      ( "Should return \"UnaryExpression(...)\"",
        "UnaryExpression(ArithmeticNegation, Literal(Integer(2)))",
        Expressions.UnaryExpression
          ( UnaryOperators.ArithmeticNegation,
            Expressions.Literal (Literals.Integer 2) ) );
      ( "Should return \"BinaryExpression(...)\"",
        "BinaryExpression(Add, Literal(Integer(5)), Literal(Integer(10)))",
        Expressions.BinaryExpression
          ( BinaryOperators.Add,
            Expressions.Literal (Literals.Integer 5),
            Expressions.Literal (Literals.Integer 10) ) );
    ]
  in
  List.iter
    (fun (msg, expected, actual) ->
      check string msg expected (Expressions.pp_expressions actual))
    to_check

let tests =
  ("Expressions", [ test_case "pp_expressions" `Quick test_pp_expressions ])