open Syntax.Roots

let get_lexbuf () =
  if Array.length Sys.argv > 1 then
    let file_path = Sys.argv.(1) in
    let file_stream = open_in file_path in
    Lexing.from_channel file_stream
  else Lexing.from_channel stdin

let () =
  let lexbuf = get_lexbuf () in
  let ast : program =
    try Analyzer.Parser.main Analyzer.Lexer.token lexbuf with
    | Analyzer.Lexer.Error c ->
        let file_name = lexbuf.lex_curr_p.pos_fname
        and line_num = lexbuf.lex_curr_p.pos_lnum
        and col_num = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        Printf.fprintf stderr "Fichier \"%s\", ligne %d, colonne %d\n%s: %s\n"
          file_name line_num col_num "Erreur lexicale"
          ("Caractère '" ^ String.make 1 c ^ "' inconnu");
        exit 1
    | Analyzer.Parser.Error ->
        let file_name = lexbuf.lex_curr_p.pos_fname
        and line_num = lexbuf.lex_curr_p.pos_lnum
        and col_num = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        Printf.fprintf stderr "Fichier \"%s\", ligne %d, colonne %d\n%s: %s\n"
          file_name line_num col_num "Erreur syntaxique" "Syntaxe incorrecte";
        exit 1
  in
  let _ = ast#check_type in
  Printf.printf "%s\n" (Syntax.Literals.string_of_literal_value ast#eval)