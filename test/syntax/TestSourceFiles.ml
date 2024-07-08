open Alcotest
open Syntax

let test_pp_source_files () =
  let to_check =
    [
      ( "Should return \"SourceFile(...)\"",
        "SourceFile(ExpressionStatement(BinaryExpression(Add, \
         Literal(Integer(1)), Literal(Integer(2)))))",
        SourceFiles.SourceFile
          (Statements.ExpressionStatement
             (Expressions.BinaryExpression
                ( BinaryOperators.Add,
                  Expressions.Literal (Literals.Integer 1),
                  Expressions.Literal (Literals.Integer 2) ))) );
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      check string name expected (SourceFiles.pp_source_files actual))
    to_check

let tests =
  ("SourceFiles", [ test_case "pp_source_files" `Quick test_pp_source_files ])