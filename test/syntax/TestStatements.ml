open Alcotest
open Syntax

let test_pp_statements () =
  let to_check =
    [
      ( "Should return \"ExpressionStatement(...)\"",
        "ExpressionStatement(Literal(Integer(1)))",
        Statements.ExpressionStatement
          (Expressions.Literal (Literals.Integer 1)) );
      ( "Should return \"SequenceStatement(...)\"",
        "SequenceStatement(ExpressionStatement(Literal(Integer(1))), \
         ExpressionStatement(Literal(Float(1.000000))))",
        Statements.SequenceStatement
          ( Statements.ExpressionStatement
              (Expressions.Literal (Literals.Integer 1)),
            Statements.ExpressionStatement
              (Expressions.Literal (Literals.Float 1.0)) ) );
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      check string name expected (Statements.pp_statements actual))
    to_check

let tests =
  ("Statements", [ test_case "pp_statements" `Quick test_pp_statements ])