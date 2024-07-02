(* lib/lexer.mll *)
{
  open Parser
  exception Error of char
}

let line_comment = "//" [^ '\n']*

let digit = ['0'-'9']
let integer = digit+

rule token = parse
   |  [' ' '\t'] | line_comment { token lexbuf }
   | ['\n'] { Lexing.new_line lexbuf; token lexbuf }

   | '+' { PLUS }
   | '-' { MINUS }
   | '*' { TIMES }
   | '/' { DIVIDE }
   | ';' { SEMICOLON }

   | integer as lxm { INT(int_of_string lxm) }

   | '(' { LPAREN }
   | ')' { RPAREN }

   | eof { EOF }
   | _ as c { raise (Error c) }