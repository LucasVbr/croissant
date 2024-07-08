open Alcotest
open Syntax.UnaryOperators

let test_pp_unary_operators () =
  let to_check =
    [
      ( "Should return \"ArithmeticNegation\"",
        ArithmeticNegation,
        "ArithmeticNegation" );
    ]
  in
  List.iter
    (fun (msg, unary_operator, expected) ->
      check string msg expected (pp_unary_operators unary_operator))
    to_check

let tests =
  ( "UnaryOperators",
    [ test_case "pp_unary_operators" `Quick test_pp_unary_operators ] )