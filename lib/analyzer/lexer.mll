{
open Parser
exception Error of char

let buffer = Buffer.create 256
}

let white_space = [' ' '\t' '\n']

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanum = letter | digit
let non_digit = '_'

let identifier = letter (alphanum | non_digit)*
let interger = digit+
let float = digit+ '.' digit+

rule token = parse
  | white_space { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }

  (* Comments *)
  | "//" { line_comment lexbuf }
  | "/*" { block_comment lexbuf }

  (* Delimiters *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ';' { SEMICOLON }

  (* Operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }

  (* Keywords *)
  (* ... *)

  (* Literals *)
  | interger as i { INT (int_of_string i) }
  | float as f { FLOAT (float_of_string f) }
  | '"' { Buffer.clear buffer; STRING(string lexbuf) }
  | "''" { NULL }
  | "'\\''"    { CHARACTER '\'' }
  | "'\\n'"    { CHARACTER '\n' }
  | "'\\t'"    { CHARACTER '\t' }
  | "'\\\\'"   { CHARACTER '\\' }
  | "'\\r'"    { CHARACTER '\r' }
  | "'\\b'"    { CHARACTER '\b' }
  | "'" [^'\\'] "'" { CHARACTER (String.get (Lexing.lexeme lexbuf) 1) }

  (* Identifiers *)
  | identifier as lxm { IDENTIFIER(lxm) }

  | eof { EOF }
  | _ as c { raise (Error c) }

and string = parse
  | '\"' { Buffer.contents buffer}
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