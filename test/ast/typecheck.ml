(* test/ast/typecheck.ml *)

open Alcotest
open Ast.Syntax
open Ast.Typecheck

let should_be_valid function_to_test tests =
  List.iter
    (fun (name, expected, source) ->
      let actual = function_to_test source in
      check (of_pp Fmt.nop) name expected actual)
    tests

let test_type_of_literal () =
  let tests =
    [
      ("1", IntegerType, Integer 1);
      ("1.0", FloatType, Float 1.0);
      ("true", BooleanType, Boolean true);
      ("false", BooleanType, Boolean false);
      ("\"hello\"", StringType, String "hello");
      ("'c'", CharacterType, Character 'c');
      ("null", VoidType, Null);
    ]
  in
  should_be_valid type_of_literal tests

let test_type_of_expression () =
  let tests =
    [
      ("42", IntegerType, Literal (Integer 42));
      ("foo", StringType, Identifier "foo");
      (* TODO Add tests *)
    ]
  and env : environment = { variables = [ ("foo", StringType) ] } in
  should_be_valid (type_of_expression env) tests
(* TODO Tests errors *)

let test_type_of_variable_declaration () =
  let tests =
    [
      ( "var bar: booleen = true;",
        { variables = [ ("bar", BooleanType) ] },
        VariableDeclaration
          (BooleanType, Identifier "bar", Literal (Boolean true)) );
    ]
  and env : environment = { variables = [] } in
  should_be_valid (type_of_variable_declaration env) tests
(* TODO check other case should return an error *)

let test_type_of_statement () =
  let tests = [ (* TODO Add Tests *) ] and env = { variables = [] } in
  should_be_valid (type_of_statement env) tests

let test_type_of_source_file () =
  let tests = [ (* TODO Add Tests *) ] in
  should_be_valid type_of_source_file tests

let () =
  run "ast.typecheck"
    [
      ( "type_of_literal",
        [ test_case "type_of_literal" `Quick test_type_of_literal ] );
      ( "type_of_expression",
        [ test_case "type_of_expression" `Quick test_type_of_expression ] );
      ( "type_of_variable_declaration",
        [
          test_case "type_of_variable_declaration" `Quick
            test_type_of_variable_declaration;
        ] );
      ( "type_of_statement",
        [ test_case "type_of_statement" `Quick test_type_of_statement ] );
      ( "type_of_source_file",
        [ test_case "type_of_source_file" `Quick test_type_of_source_file ] );
    ]