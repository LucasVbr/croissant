(* bin/main.ml *)

open Printf
open Ast.Print

let get_lexbuf () =
  if Array.length Sys.argv > 1 then (
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    Lexing.set_filename lexbuf Sys.argv.(1);
    lexbuf)
  else
    let lexbuf = Lexing.from_channel stdin in
    Lexing.set_filename lexbuf "stdin";
    lexbuf

let () =
  let lexbuf = get_lexbuf () in
  let res =
    try Parser.main Lexer.token lexbuf with
    | Lexer.Error c ->
        let file_name = lexbuf.lex_curr_p.pos_fname
        and line_num = lexbuf.lex_curr_p.pos_lnum
        and col_num = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        fprintf stderr "Fichier \"%s\", ligne %d, colonne %d\n%s: %s\n"
          file_name line_num col_num "Erreur lexicale"
          ("Caractère '" ^ String.make 1 c ^ "' inconnu");
        exit 1
    | Parser.Error ->
        let file_name = lexbuf.lex_curr_p.pos_fname
        and line_num = lexbuf.lex_curr_p.pos_lnum
        and col_num = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        fprintf stderr "Fichier \"%s\", ligne %d, colonne %d\n%s: %s\n"
          file_name line_num col_num "Erreur syntaxique" "Syntaxe incorrecte";
        exit 1
  in
  let _ = res in
  Printf.printf "%s\n" (string_of_source_file res)