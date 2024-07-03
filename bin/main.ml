(* bin/main.ml *)

open Printf
open Ast.Print

exception Error of string

let () =
  (*  Array.iteri (fun i arg -> printf "%d: %s\n" i arg) Sys.argv; *)
  let help_message = "Usage: croissant <file_path>\n"
  and args_count = Array.length Sys.argv - 1 in
  if args_count >= 2 then printf "%s" help_message
  else if args_count = 1 then
    let file_path = Sys.argv.(1) in
    let ast = string_of_source_file (Analyzer.analyze_file file_path) in
    printf "%s\n" ast
  else raise (Error "interpreter from stdin is not implemented yet")
(* TODO: Implement interpreter from stdin *)