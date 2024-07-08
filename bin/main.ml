let () =
  let file_path = Sys.argv.(1) in
  let file_stream = open_in file_path in
  let lexbuf = Lexing.from_channel file_stream in
  let ast =
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
  Printf.printf "%s\n" (Syntax.SourceFiles.pp_source_files ast)