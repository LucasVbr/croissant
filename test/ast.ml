(* test/ast.ml *)

open Alcotest
open Ast

let test_string_of_binary_operator () =
  check string "+" "Add" (string_of_binary_operator Add);
  check string "-" "Substract" (string_of_binary_operator Substract);
  check string "*" "Multiply" (string_of_binary_operator Multiply);
  check string "/" "Divide" (string_of_binary_operator Divide)

let test_string_of_expression () =
  let expr = BinaryExpression (Add, IntegerLiteral 1, IntegerLiteral 2) in
  check string "1 + 2"
    "BinaryExpression(Add, IntegerLiteral(1), IntegerLiteral(2))"
    (string_of_expression expr)

let test_string_of_statement () =
  let stmt = ExpressionStatement (IntegerLiteral 42) in
  check string "42;" "ExpressionStatement(IntegerLiteral(42))"
    (string_of_statement stmt)

let test_string_of_source_file () =
  let source_file =
    SourceFile
      [
        ExpressionStatement (IntegerLiteral 1);
        ExpressionStatement
          (BinaryExpression (Add, IntegerLiteral 2, IntegerLiteral 3));
      ]
  in
  check string "1; 2 + 3;"
    "SourceFile([ExpressionStatement(IntegerLiteral(1)), \
     ExpressionStatement(BinaryExpression(Add, IntegerLiteral(2), \
     IntegerLiteral(3)))])"
    (string_of_source_file source_file)

let () =
  let open Alcotest in
  run "AST tests"
    [
      ( "string_of_binary_operator",
        [
          test_case "string_of_binary_operator" `Quick
            test_string_of_binary_operator;
        ] );
      ( "string_of_expression",
        [ test_case "string_of_expression" `Quick test_string_of_expression ] );
      ( "string_of_statement",
        [ test_case "string_of_statement" `Quick test_string_of_statement ] );
      ( "string_of_source_file",
        [ test_case "string_of_source_file" `Quick test_string_of_source_file ]
      );
    ]