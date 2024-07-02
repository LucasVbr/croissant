(* test/print.ml *)

open Alcotest
open Ast.Syntax
open Ast.Print

let test_string_of_type () =
  check string "int" "Type_Integer" (string_of_type Type_Integer);
  check string "float" "Type_Float" (string_of_type Type_Float);
  check string "bool" "Type_Boolean" (string_of_type Type_Boolean);
  check string "string" "Type_String" (string_of_type Type_String);
  check string "char" "Type_Character" (string_of_type Type_Character);
  check string "void" "Type_Void" (string_of_type Type_Void)

let test_string_of_literal () =
  check string "42" "Integer(42)" (string_of_literal (Integer 42));
  check string "3.14" "Float(3.14)" (string_of_literal (Float 3.14));
  check string "true" "Boolean(true)" (string_of_literal (Boolean true));
  check string "false" "Boolean(false)" (string_of_literal (Boolean false));
  check string "hello" "String(\"hello\")" (string_of_literal (String "hello"));
  check string "c" "Character('c')" (string_of_literal (Character 'c'));
  check string "null" "Null" (string_of_literal Null)

let test_string_of_binary_operator () =
  check string "+" "Add" (string_of_binary_operator Add);
  check string "-" "Substract" (string_of_binary_operator Substract);
  check string "*" "Multiply" (string_of_binary_operator Multiply);
  check string "/" "Divide" (string_of_binary_operator Divide);
  check string "&&" "AmpersandAmpersand"
    (string_of_binary_operator AmpersandAmpersand);
  check string "||" "BarBar" (string_of_binary_operator BarBar);
  check string "==" "EqualsEquals" (string_of_binary_operator EqualsEquals);
  check string "!=" "ExclamationEquals"
    (string_of_binary_operator ExclamationEquals);
  check string "<" "LessThan" (string_of_binary_operator LessThan);
  check string "<=" "LessThanEquals" (string_of_binary_operator LessThanEquals);
  check string ">" "GreaterThan" (string_of_binary_operator GreaterThan);
  check string ">=" "GreaterThanEquals"
    (string_of_binary_operator GreaterThanEquals)

let test_string_of_unary_operator () =
  check string "-" "Negate" (string_of_unary_operator Negate);
  check string "!" "Not" (string_of_unary_operator Not)

let test_string_of_expression () =
  let expr = Literal (Integer 42) in
  check string "42" "Literal(Integer(42))" (string_of_expression expr);
  let expr = BinaryExpression (Add, Literal (Integer 1), Literal (Integer 2)) in
  check string "1 + 2"
    "BinaryExpression(Add, Literal(Integer(1)), Literal(Integer(2)))"
    (string_of_expression expr);
  let expr = UnaryExpression (Negate, Literal (Integer 42)) in
  check string "-42" "UnaryExpression(Negate, Literal(Integer(42)))"
    (string_of_expression expr);
  let expr = Identifier "x" in
  check string "x" "Identifier(\"x\")" (string_of_expression expr)

let test_string_of_statement () =
  let stmt = ExpressionStatement (Literal (Integer 42)) in
  check string "42;" "ExpressionStatement(Literal(Integer(42)))"
    (string_of_statement stmt);
  let stmt =
    VariableStatement
      [ VariableDeclaration (Type_Integer, Identifier "x", Literal Null) ]
  in
  check string "int x;"
    "VariableStatement([VariableDeclaration(Type_Integer, Identifier(\"x\"), \
     Literal(Null))])"
    (string_of_statement stmt);
  let stmt =
    VariableStatement
      [
        VariableDeclaration (Type_Integer, Identifier "x", Literal (Integer 42));
      ]
  in
  check string "int x = 42;"
    "VariableStatement([VariableDeclaration(Type_Integer, Identifier(\"x\"), \
     Literal(Integer(42)))])"
    (string_of_statement stmt)

let test_string_of_source_file () =
  let source_file =
    SourceFile
      [
        ExpressionStatement (Literal (Integer 1));
        ExpressionStatement
          (BinaryExpression (Add, Literal (Integer 2), Literal (Integer 3)));
      ]
  in
  check string "1; 2 + 3;"
    "SourceFile([ExpressionStatement(Literal(Integer(1))), \
     ExpressionStatement(BinaryExpression(Add, Literal(Integer(2)), \
     Literal(Integer(3))))])"
    (string_of_source_file source_file)

let () =
  let open Alcotest in
  run "AST tests"
    [
      ("string_of_type", [ test_case "type" `Quick test_string_of_type ]);
      ( "string_of_literal",
        [ test_case "literal" `Quick test_string_of_literal ] );
      ( "string_of_unary_operator",
        [ test_case "unary_operator" `Quick test_string_of_unary_operator ] );
      ( "string_of_binary_operator",
        [ test_case "binary_operator" `Quick test_string_of_binary_operator ] );
      ( "string_of_expression",
        [ test_case "expression" `Quick test_string_of_expression ] );
      ( "string_of_statement",
        [ test_case "statement" `Quick test_string_of_statement ] );
      ( "string_of_source_file",
        [ test_case "source_file" `Quick test_string_of_source_file ] );
    ]