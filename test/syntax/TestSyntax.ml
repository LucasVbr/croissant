open Alcotest

let () =
  run "Syntax"
    [
      TestTypes.tests;
      TestLiterals.tests;
      TestUnaryOperators.tests;
      TestBinaryOperators.tests;
      TestExpressions.tests;
      TestStatements.tests;
      TestSourceFiles.tests;
    ]