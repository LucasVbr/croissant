open Printf

let analyze lexbuf =
  let ast =
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
  ast

let analyze_file file_name =
  let file_stream = open_in file_name in
  let lexbuf = Lexing.from_channel file_stream in
  Lexing.set_filename lexbuf file_name;
  analyze lexbuf