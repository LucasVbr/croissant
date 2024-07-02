(* lib/lexer.mll *)
{
  open Parser
  exception Error of char
}

let line_comment = "//" [^ '\n']*

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanum = letter | digit
let non_digit = '_'

let identifier = letter (alphanum | non_digit)*
let integer = digit+
let float = digit* '.' digit+
let char = "'" [^'.'] "'"
let string = '"' [^'.']*  '"'

rule token = parse
   |  [' ' '\t'] | line_comment { token lexbuf }
   | ['\n'] { Lexing.new_line lexbuf; token lexbuf }

   | '+' { PLUS }
   | '-' { MINUS }
   | '*' { TIMES }
   | '/' { DIVIDE }
   | ';' { SEMICOLON }
   | "&&" { AMPERSAND_AMPERSAND }
   | "||" { BAR_BAR }
   | "==" { EQUALS_EQUALS }
   | "!=" { EXCLAMATION_EQUALS }
   | '<' { LESS_THAN }
   | "<=" { LESS_THAN_EQUALS }
   | '>' { GREATER_THAN }
   | ">=" { GREATER_THAN_EQUALS }

   | "!" { EXCLAMATION }

   | '(' { LPAREN }
   | ')' { RPAREN }

   | "vrai" { BOOLEAN(true) }
   | "faux" { BOOLEAN(false) }
   | "vide" { NULL }
   | integer as lxm { INTEGER(int_of_string lxm) }
   | float as lxm { FLOAT(float_of_string lxm) }
   | char as lxm { CHARACTER(lxm.[1]) }
   | string as lxm { STRING(String.sub lxm 1 (String.length lxm - 2)) }
   | identifier as lxm { IDENTIFIER(lxm) }

   | eof { EOF }
   | _ as c { raise (Error c) }