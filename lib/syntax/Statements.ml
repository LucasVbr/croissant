open Expressions

type statements =
  | SequenceStatement of statements * statements
  | ExpressionStatement of expressions

let rec pp_statements = function
  | SequenceStatement (stmt1, stmt2) ->
      let pp_stmt1 = pp_statements stmt1 and pp_stmt2 = pp_statements stmt2 in
      Printf.sprintf "SequenceStatement(%s, %s)" pp_stmt1 pp_stmt2
  | ExpressionStatement expr ->
      let pp_expr = pp_expressions expr in
      Printf.sprintf "ExpressionStatement(%s)" pp_expr
