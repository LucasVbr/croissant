/* lib/parser.mly */
%{
   open Ast.Syntax
%}

%token <int> INTEGER
%token <float> FLOAT
%token <bool> BOOLEAN
%token <char> CHARACTER
%token <string> STRING
%token <string> IDENTIFIER
%token NULL

%token INTEGER_TYPE
%token FLOAT_TYPE
%token CHARACTER_TYPE
%token STRING_TYPE
%token BOOLEAN_TYPE
%token VOID_TYPE

%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIVIDE "/"
%token AMPERSAND_AMPERSAND "&&"
%token BAR_BAR "||"
%token EXCLAMATION "!"
%token EQUALS_EQUALS "=="
%token EXCLAMATION_EQUALS "!="
%token LESS_THAN "<"
%token LESS_THAN_EQUALS "<="
%token GREATER_THAN ">"
%token GREATER_THAN_EQUALS ">="
%token EQUALS "="

%token LPAREN "("
%token RPAREN ")"

%token SEMICOLON ";"
%token COLON ":"
%token COMMA ","

%token VAR "var"

%token EOF

%left "&&" "||"
%left "+" "-"
%left "*" "/"
%nonassoc UMINUS

%start main
%type <source_file> main

%%

main:
  | statements EOF { SourceFile($1) }
  | EOF { SourceFile([]) }

statements:
  | statement ";" { [$1] }
  | statement ";" statements { $1 :: $3 }

statement:
  | expression { ExpressionStatement($1) }
  | "var" variable_declaration_list { VariableStatement($2) }

variable_declaration_list:
  | variable_declaration { [$1] }
  | variable_declaration "," variable_declaration_list { $1 :: $3 }

variable_declaration:
  | IDENTIFIER ":" tp "=" expression { VariableDeclaration($3, Identifier($1), $5) }
  | IDENTIFIER ":" tp { VariableDeclaration($3, Identifier($1), Literal(Null)) }

tp:
  | INTEGER_TYPE { IntegerType }
  | FLOAT_TYPE { FloatType }
  | CHARACTER_TYPE { CharacterType }
  | STRING_TYPE { StringType }
  | BOOLEAN_TYPE { BooleanType }
  | VOID_TYPE { VoidType }

expression:
  | literal { Literal($1) }
  | IDENTIFIER { Identifier($1) }
  | unary_expression { $1 }
  | binary_expression { $1 }
  | "(" expression ")" { $2 }

literal:
  | INTEGER { Integer($1) }
  | FLOAT { Float($1) }
  | CHARACTER { Character($1) }
  | STRING { String($1) }
  | BOOLEAN { Boolean($1) }
  | NULL { Null }

unary_expression:
 | "-" expression %prec UMINUS { UnaryExpression(Negate, $2) }
 | "!" expression { UnaryExpression(Not, $2) }

binary_expression:
  | e1=expression "+" e2=expression { BinaryExpression(Add, e1, e2) }
  | e1=expression "-" e2=expression { BinaryExpression(Substract, e1, e2) }
  | e1=expression "*" e2=expression { BinaryExpression(Multiply, e1, e2) }
  | e1=expression "/" e2=expression { BinaryExpression(Divide, e1, e2) }
  | e1=expression "&&" e2=expression { BinaryExpression(AmpersandAmpersand, e1, e2) }
  | e1=expression "||" e2=expression { BinaryExpression(BarBar, e1, e2) }
  | e1=expression "==" e2=expression { BinaryExpression(EqualsEquals, e1, e2) }
  | e1=expression "!=" e2=expression { BinaryExpression(ExclamationEquals, e1, e2) }
  | e1=expression "<" e2=expression { BinaryExpression(LessThan, e1, e2) }
  | e1=expression "<=" e2=expression { BinaryExpression(LessThanEquals, e1, e2) }
  | e1=expression ">" e2=expression { BinaryExpression(GreaterThan, e1, e2) }
  | e1=expression ">=" e2=expression { BinaryExpression(GreaterThanEquals, e1, e2) }
