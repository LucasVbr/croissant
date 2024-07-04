(* lib/ast/syntax.ml *)

open Syntax

exception Type_error of string

type environment = { variables : (string * _type) list }

(** [type_of_literal lit] returns the type of the given literal. *)
let type_of_literal (lit : literal) : _type =
  match lit with
  | Integer _ -> IntegerType
  | Float _ -> FloatType
  | Character _ -> CharacterType
  | String _ -> StringType
  | Boolean _ -> BooleanType
  | Null -> VoidType

(** [type_of_expression env expr] returns the type of the given expression. *)
let rec type_of_expression (env : environment) (expr : expression) : _type =
  match expr with
  | Literal lit -> type_of_literal lit
  | Identifier id -> (
      try List.assoc id env.variables
      with Not_found -> raise (Type_error ("Unknown variable " ^ id)))
  | UnaryExpression (op, expr) -> (
      let expr_type = type_of_expression env expr in
      match op with
      | Negate -> (
          match expr_type with
          | IntegerType | FloatType -> expr_type
          | _ ->
              raise
                (Type_error "Negate operator can only be applied to numbers"))
      | Not -> (
          match expr_type with
          | BooleanType -> BooleanType
          | _ ->
              raise (Type_error "Not operator can only be applied to booleans"))
      )
  | BinaryExpression (op, left, right) -> (
      let left_type = type_of_expression env left in
      let right_type = type_of_expression env right in
      match op with
      | Add | Subtract | Multiply | Divide -> (
          match (left_type, right_type) with
          | IntegerType, IntegerType -> IntegerType
          | FloatType, FloatType -> FloatType
          | IntegerType, FloatType | FloatType, IntegerType -> FloatType
          | _ -> raise (Type_error "Arithmetic operations require numbers"))
      | AmpersandAmpersand | BarBar -> (
          match (left_type, right_type) with
          | BooleanType, BooleanType -> BooleanType
          | _ -> raise (Type_error "Logical operations require booleans"))
      | EqualsEquals | ExclamationEquals -> BooleanType
      | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals -> (
          match (left_type, right_type) with
          | IntegerType, IntegerType | FloatType, FloatType -> BooleanType
          | IntegerType, FloatType | FloatType, IntegerType -> BooleanType
          | _ -> raise (Type_error "Comparison operations require numbers"))
      | Assign -> (
          match left with
          | Identifier _ ->
              if left_type = right_type then left_type
              else raise (Type_error "Assignment requires matching types")
          | _ ->
              raise
                (Type_error
                   "Assignment requires a variable on the left-hand side")))

(** [type_of_variable_declaration env var_decl] returns the environment after
    processing the given variable declaration. *)
let type_of_variable_declaration (env : environment)
    (VariableDeclaration (_type, id_expr, expr)) =
  match id_expr with
  | Identifier id ->
      let expr_type = type_of_expression env expr in
      if expr_type = _type then { variables = (id, _type) :: env.variables }
      else
        raise (Type_error ("Type mismatch in variable declaration for " ^ id))
  | _ -> raise (Type_error "Variable name must be an identifier")

(** [type_of_statement env stmt] returns the environment after processing the
    given statement. *)
let type_of_statement (env : environment) (stmt : statement) =
  match stmt with
  | ExpressionStatement expr ->
      let _ = type_of_expression env expr in
      env
  | VariableStatement var_decls ->
      List.fold_left type_of_variable_declaration env var_decls

(** [type_of_source_file (SourceFile stmts)] returns the environment after
    processing the given source file. *)
let type_of_source_file (SourceFile stmts) =
  let initial_env = { variables = [] } in
  List.fold_left type_of_statement initial_env stmts