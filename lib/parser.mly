/* lib/parser.mly */
%{
   open Ast
%}

%token <int> INT

%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIVIDE "/"

%token LPAREN "("
%token RPAREN ")"

%token SEMICOLON ";"

%token EOF

%left "+" "-"
%left "*" "/"
//%nonassoc UMINUS

%start main
%type <source_file> main

%%

main:
  | statements EOF { SourceFile($1) }

statements:
  | statement ";" { [$1] }
  | statement ";" statements { $1 :: $3 }

statement:
  | expression { ExpressionStatement($1) }

expression:
  | literal { $1 }
  | binary_expression { $1 }
  | "(" expression ")" { $2 }

literal:
  | INT { IntegerLiteral($1) }

binary_expression:
  | e1=expression PLUS e2=expression { BinaryExpression(Add, e1, e2) }
  | e1=expression MINUS e2=expression { BinaryExpression(Substract, e1, e2) }
  | e1=expression TIMES e2=expression { BinaryExpression(Multiply, e1, e2) }
  | e1=expression DIVIDE e2=expression { BinaryExpression(Divide, e1, e2) }