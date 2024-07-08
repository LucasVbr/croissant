open Alcotest
open Syntax.BinaryOperators

let test_pp_binary_operators () =
  let to_check =
    [
      ("Should return \"Add\"", "Add", Add);
      ("Should return \"Subtract\"", "Subtract", Subtract);
      ("Should return \"Multiply\"", "Multiply", Multiply);
      ("Should return \"Divide\"", "Divide", Divide);
    ]
  in
  List.iter
    (fun (msg, expected, actual) ->
      check string msg expected (pp_binary_operators actual))
    to_check

let tests =
  ( "BinaryOperators",
    [ test_case "pp_binary_operators" `Quick test_pp_binary_operators ] )