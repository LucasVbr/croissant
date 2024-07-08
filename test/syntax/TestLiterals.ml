open Alcotest
open Syntax.Literals

let test_pp_literals () =
  let to_check =
    [
      ("Should return \"Integer(1)\"", "Integer(1)", Integer 1);
      ("Should return \"Float(1.000000)\"", "Float(1.000000)", Float 1.0);
      ( "Should return \"String(\\\"Hello, World!\\\")\"",
        "String(\"Hello, World!\")",
        String "Hello, World!" );
      ("Should return \"Character('c')\"", "Character('c')", Character 'c');
      ("Should return \"Boolean(true)\"", "Boolean(true)", Boolean true);
      ("Should return \"Boolean(false)\"", "Boolean(false)", Boolean false);
    ]
  in
  List.iter
    (fun (msg, expected, actual) ->
      check string msg expected (pp_literals actual))
    to_check

let tests = ("Literals", [ test_case "pp_literals" `Quick test_pp_literals ])