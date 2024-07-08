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
(*  | float as f { FLOAT (float_of_string f) } *)

  (* Identifiers *)
  (* ... *)

  | eof { EOF }
  | _ as c { raise (Error c) }

and string = parse
  | '"' { token lexbuf }
  | '\\' { Buffer.add_char buffer '\\'; string lexbuf }
  | '\n' { Buffer.add_char buffer '\n'; string lexbuf }
  | eof { raise (Error '"') }
  | _ { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0); string lexbuf }

and line_comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { line_comment lexbuf }

and block_comment = parse
  | "*/" { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof { EOF }
  | _ { block_comment lexbuf }