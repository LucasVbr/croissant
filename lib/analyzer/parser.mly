%{
open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHARACTER
%token <bool> BOOLEAN
%token <string> STRING
%token <string> IDENTIFIER
%token NULL

%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token EQUAL_EQUAL "=="
%token NOT_EQUAL "!="
%token LESS_THAN "<"
%token LESS_THAN_EQUAL "<="
%token GREATER_THAN ">"
%token GREATER_THAN_EQUAL ">="
%token AMPERSAND_AMPERSAND "&&"
%token BAR_BAR "||"
%token EXCLAMATION "!"

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
  | BOOLEAN { Literals.Boolean($1) }
  | CHARACTER { Literals.Character($1) }
  | STRING { Literals.String($1) }
  | NULL { Literals.Null }

unary_expression:
  | "-" expression %prec UMINUS { Expressions.UnaryExpression(UnaryOperators.ArithmeticNegation, $2) }
  | "!" expression { Expressions.UnaryExpression(UnaryOperators.LogicalNegation, $2) }

binary_expression:
  | expression "+" expression { Expressions.BinaryExpression (BinaryOperators.Add, $1, $3) }
  | expression "-" expression { Expressions.BinaryExpression (BinaryOperators.Subtract, $1, $3) }
  | expression "*" expression { Expressions.BinaryExpression (BinaryOperators.Multiply, $1, $3) }
  | expression "/" expression { Expressions.BinaryExpression (BinaryOperators.Divide, $1, $3) }
  | expression "==" expression { Expressions.BinaryExpression (BinaryOperators.Equals, $1, $3) }
  | expression "!=" expression { Expressions.BinaryExpression (BinaryOperators.NotEquals, $1, $3) }
  | expression "<" expression { Expressions.BinaryExpression (BinaryOperators.LessThan, $1, $3) }
  | expression "<=" expression { Expressions.BinaryExpression (BinaryOperators.LessThanEquals, $1, $3) }
  | expression ">" expression { Expressions.BinaryExpression (BinaryOperators.GreaterThan, $1, $3) }
  | expression ">=" expression { Expressions.BinaryExpression (BinaryOperators.GreaterThanEquals, $1, $3) }
  | expression "&&" expression { Expressions.BinaryExpression (BinaryOperators.And, $1, $3) }
  | expression "||" expression { Expressions.BinaryExpression (BinaryOperators.Or, $1, $3) }