open Statements

type source_files = SourceFile of statements

let pp_source_files = function
  | SourceFile stmts ->
      let pp_stmt = pp_statements stmts in
      Printf.sprintf "SourceFile(%s)" pp_stmt