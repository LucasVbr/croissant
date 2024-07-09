open Literals
open UnaryOperators
open BinaryOperators

type expressions =
  | Literal of literals
  | Identifier of string
  | UnaryExpression of unary_operators * expressions
  | BinaryExpression of binary_operators * expressions * expressions

let rec pp_expressions = function
  | Literal l ->
      let pp_l = pp_literals l in
      Printf.sprintf "Literal(%s)" pp_l
  | Identifier i -> Printf.sprintf "Identifier(\"%s\")" i
  | UnaryExpression (op, e) ->
      let pp_op = pp_unary_operators op and pp_e = pp_expressions e in
      Printf.sprintf "UnaryExpression(%s, %s)" pp_op pp_e
  | BinaryExpression (op, e1, e2) ->
      let pp_op = pp_binary_operators op
      and pp_e1 = pp_expressions e1
      and pp_e2 = pp_expressions e2 in
      Printf.sprintf "BinaryExpression(%s, %s, %s)" pp_op pp_e1 pp_e2