open Alcotest
open Syntax.Types

let test_pp_types () =
  let to_check =
    [
      ("Should return \"IntegerType\"", "IntegerType", IntegerType);
      ("Should return \"BooleanType\"", "BooleanType", BooleanType);
      ("Should return \"StringType\"", "StringType", StringType);
      ("Should return \"CharacterType\"", "CharacterType", CharacterType);
      ("Should return \"VoidType\"", "VoidType", VoidType);
    ]
  in
  List.iter
    (fun (msg, expected, input) -> check string msg expected (pp_types input))
    to_check

let tests = ("Types", [ test_case "pp_types" `Quick test_pp_types ])