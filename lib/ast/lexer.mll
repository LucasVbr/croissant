(* lib/lexer.mll *)
{
  open Parser
  exception Error of char

  let buffer = Buffer.create 256
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanum = letter | digit
let non_digit = '_'

let identifier = letter (alphanum | non_digit)*
let integer = digit+
let float = digit* '.' digit+

rule token = parse
   |  ' ' | '\t' { token lexbuf }
   | "/*" { block_comment lexbuf }
   | "//" { line_comment lexbuf }
   | '\n' { Lexing.new_line lexbuf; token lexbuf }

   | '+' { PLUS }
   | '-' { MINUS }
   | '*' { TIMES }
   | '/' { DIVIDE }
   | "&&" { AMPERSAND_AMPERSAND }
   | "||" { BAR_BAR }
   | "==" { EQUALS_EQUALS }
   | "!=" { EXCLAMATION_EQUALS }
   | '<' { LESS_THAN }
   | "<=" { LESS_THAN_EQUALS }
   | '>' { GREATER_THAN }
   | ">=" { GREATER_THAN_EQUALS }
   | '=' { EQUALS }

   | "!" { EXCLAMATION }
   | ';' { SEMICOLON }
   | ':' { COLON }
   | ',' { COMMA }

   | '(' { LPAREN }
   | ')' { RPAREN }

   | "var" { VAR }
   | "vrai" { BOOLEAN(true) }
   | "faux" { BOOLEAN(false) }
   | "nul" { NULL }

   | "entier" { INTEGER_TYPE }
   | "reel" { FLOAT_TYPE }
   | "caractere" { CHARACTER_TYPE }
   | "chaine" { STRING_TYPE }
   | "booleen" { BOOLEAN_TYPE }
   | "vide" { VOID_TYPE }

   | integer as lxm { INTEGER(int_of_string lxm) }
   | float as lxm { FLOAT(float_of_string lxm) }

  | "'\\''"    { CHARACTER '\'' }
  | "'\\n'"    { CHARACTER '\n' }
  | "'\\t'"    { CHARACTER '\t' }
  | "'\\\\'"   { CHARACTER '\\' }
  | "'\\r'"    { CHARACTER '\r' }
  | "'\\b'"    { CHARACTER '\b' }
  | "'" [^'\\'] "'" { CHARACTER (String.get (Lexing.lexeme lexbuf) 1) }

   | "\"" { Buffer.clear buffer; string lexbuf }
   | identifier as lxm { IDENTIFIER(lxm) }

   | eof { EOF }
   | _ as c { raise (Error c) }

and string = parse
  | "\"" { STRING(Buffer.contents buffer)}
  | "\\\"" { Buffer.add_char buffer '"'; string lexbuf }
  | '\\' { Buffer.add_char buffer '\\'; string lexbuf }
  | _ as c { Buffer.add_char buffer c; string lexbuf }

and line_comment = parse
 | '\n' { Lexing.new_line lexbuf; token lexbuf }
 | eof { EOF }
 | _ { line_comment lexbuf }

and block_comment = parse
 | "*/" { token lexbuf }
 | '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
 | eof { EOF }
 | _ { block_comment lexbuf }