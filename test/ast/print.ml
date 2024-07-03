(* test/print.ml *)

open Alcotest
open Ast.Syntax

module To_test = struct
  let _type = Ast.Print.string_of_type
  let literal = Ast.Print.string_of_literal
  let binary_operator = Ast.Print.string_of_binary_operator
  let unary_operator = Ast.Print.string_of_unary_operator
  let expression = Ast.Print.string_of_expression
  let statement = Ast.Print.string_of_statement
  let source_file = Ast.Print.string_of_source_file
end

let test_string_of_type () =
  let tests =
    [
      ("int", "IntegerType", IntegerType);
      ("float", "FloatType", FloatType);
      ("char", "CharacterType", CharacterType);
      ("string", "StringType", StringType);
      ("bool", "BooleanType", BooleanType);
      ("void", "VoidType", VoidType);
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test._type actual))
    tests

let test_string_of_literal () =
  let tests =
    [
      ("42", "Integer(42)", Integer 42);
      ("3.14", "Float(3.14)", Float 3.14);
      ("true", "Boolean(true)", Boolean true);
      ("false", "Boolean(false)", Boolean false);
      ("hello", "String(\"hello\")", String "hello");
      ("c", "Character('c')", Character 'c');
      ("null", "Null", Null);
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.literal actual))
    tests

let test_string_of_binary_operator () =
  let tests =
    [
      ("+", "Add", Add);
      ("-", "Substract", Substract);
      ("*", "Multiply", Multiply);
      ("/", "Divide", Divide);
      ("&&", "AmpersandAmpersand", AmpersandAmpersand);
      ("||", "BarBar", BarBar);
      ("==", "EqualsEquals", EqualsEquals);
      ("!=", "ExclamationEquals", ExclamationEquals);
      ("<", "LessThan", LessThan);
      ("<=", "LessThanEquals", LessThanEquals);
      (">", "GreaterThan", GreaterThan);
      (">=", "GreaterThanEquals", GreaterThanEquals);
      ("=", "Assign", Assign);
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.binary_operator actual))
    tests

let test_string_of_unary_operator () =
  let tests = [ ("-", "Negate", Negate); ("!", "Not", Not) ] in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.unary_operator actual))
    tests

let test_string_of_expression () =
  let tests =
    [
      ("42", "Literal(Integer(42))", Literal (Integer 42));
      ( "1 + 2",
        "BinaryExpression(Add, Literal(Integer(1)), Literal(Integer(2)))",
        BinaryExpression (Add, Literal (Integer 1), Literal (Integer 2)) );
      ( "-42",
        "UnaryExpression(Negate, Literal(Integer(42)))",
        UnaryExpression (Negate, Literal (Integer 42)) );
      ("x", "Identifier(\"x\")", Identifier "x");
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.expression actual))
    tests

let test_string_of_statement () =
  let tests =
    [
      ( "42;",
        "ExpressionStatement(Literal(Integer(42)))",
        ExpressionStatement (Literal (Integer 42)) );
      ( "int x;",
        "VariableStatement([VariableDeclaration(IntegerType, \
         Identifier(\"x\"), Literal(Null))])",
        VariableStatement
          [ VariableDeclaration (IntegerType, Identifier "x", Literal Null) ] );
      ( "int x = 42;",
        "VariableStatement([VariableDeclaration(IntegerType, \
         Identifier(\"x\"), Literal(Integer(42)))])",
        VariableStatement
          [
            VariableDeclaration
              (IntegerType, Identifier "x", Literal (Integer 42));
          ] );
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.statement actual))
    tests

let test_string_of_source_file () =
  let tests =
    [
      ( "1; 2 + 3;",
        "SourceFile([ExpressionStatement(Literal(Integer(1))), \
         ExpressionStatement(BinaryExpression(Add, Literal(Integer(2)), \
         Literal(Integer(3))))])",
        SourceFile
          [
            ExpressionStatement (Literal (Integer 1));
            ExpressionStatement
              (BinaryExpression (Add, Literal (Integer 2), Literal (Integer 3)));
          ] );
    ]
  in
  List.iter
    (fun (name, expected, actual) ->
      (check string) name expected (To_test.source_file actual))
    tests

let () =
  run "ast.print"
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