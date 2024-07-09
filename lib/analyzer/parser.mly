%{
open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHARACTER
%token <string> STRING
%token <string> IDENTIFIER
%token NULL

%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"

%token SEMICOLON ";"

%token LPAREN "("
%token RPAREN ")"

%token EOF

%left PLUS MINUS
%left TIMES DIV

%nonassoc UMINUS

%start <SourceFiles.source_files> main

%%

main:
  | statements EOF { SourceFiles.SourceFile $1 }

statements:
  | statement ";"            { $1 }
  | statement ";" statements { Statements.SequenceStatement ($1, $3) }

statement:
  | expression  { Statements.ExpressionStatement($1) }

expression:
  | literal            { Expressions.Literal($1) }
  | IDENTIFIER         { Expressions.Identifier($1) }
  | unary_expression   { $1 }
  | binary_expression  { $1 }
  | "(" expression ")" { $2 }

literal:
  | INT { Literals.Integer($1) }
  | FLOAT { Literals.Float($1) }
  | CHARACTER { Literals.Character($1) }
  | STRING { Literals.String($1) }
  | NULL { Literals.Null }

unary_expression:
  | "-" expression %prec UMINUS { Expressions.UnaryExpression(UnaryOperators.ArithmeticNegation, $2) }

binary_expression:
  | expression "+" expression { Expressions.BinaryExpression (BinaryOperators.Add, $1, $3) }
  | expression "-" expression { Expressions.BinaryExpression (BinaryOperators.Subtract, $1, $3) }
  | expression "*" expression { Expressions.BinaryExpression (BinaryOperators.Multiply, $1, $3) }
  | expression "/" expression { Expressions.BinaryExpression (BinaryOperators.Divide, $1, $3) }