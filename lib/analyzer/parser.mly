%{
open Syntax
%}

%token <int> INTEGER
%token <float> FLOAT
%token <char> CHARACTER
%token <bool> BOOLEAN
%token <string> STRING
%token <string> IDENTIFIER

%token EQUAL "="
%token VAR

%token INTEGER_TYPE
%token FLOAT_TYPE
%token BOOLEAN_TYPE
%token STRING_TYPE
%token CHARACTER_TYPE
%token NULL_TYPE
%token VOID_TYPE

%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIVIDE "/"
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
%token COLON ":"

%token LPAREN "("
%token RPAREN ")"

%token EOF

%left PLUS MINUS
%left TIMES DIV

%nonassoc UMINUS

%start <Roots.program> main

%%

main:
  | statements EOF { new Roots.program $1 }

statements:
  | statement ";"            { $1 }
  | statement ";" statements { new Statements.sequence $1 $3 }

statement:
  | expression  { new Statements.expression $1 }
  | VAR IDENTIFIER ":" tp "=" expression { new Statements.variableDeclaration $2 $4 $6 }

expression:
  | literal            { new Expressions.literal $1 }
  | IDENTIFIER         { new Expressions.identifier $1 }
  | unary_expression   { $1 }
  | binary_expression  { $1 }
  | "(" expression ")" { $2 }

tp:
  | INTEGER_TYPE { Types.IntegerType }
  | FLOAT_TYPE { Types.FloatType }
  | BOOLEAN_TYPE { Types.BooleanType }
  | STRING_TYPE { Types.StringType }
  | CHARACTER_TYPE { Types.CharacterType }
  | NULL_TYPE { Types.NullType }
  | VOID_TYPE { Types.VoidType }

literal:
  | INTEGER { new Literals.eInteger $1 }
  | FLOAT { new Literals.eFloat $1 }
  | BOOLEAN { new Literals.eBoolean $1 }
  | CHARACTER { new Literals.eCharacter $1 }
  | STRING { new Literals.eString $1 }
  | NULL_TYPE { new Literals.eNull }

unary_expression:
  | "-" expression %prec UMINUS { new Expressions.unaryOperation (new UnaryOperators.arithmeticNegation) $2 }
  | "!" expression              { new Expressions.unaryOperation (new UnaryOperators.logicalNegation) $2 }

binary_expression:
  | expression "+" expression  { new Expressions.binaryOperation (new BinaryOperators.add) $1 $3 }
  | expression "-" expression  { new Expressions.binaryOperation (new BinaryOperators.subtract) $1 $3 }
  | expression "*" expression  { new Expressions.binaryOperation (new BinaryOperators.multiply) $1 $3 }
  | expression "/" expression  { new Expressions.binaryOperation (new BinaryOperators.divide) $1 $3 }
  | expression "==" expression { new Expressions.binaryOperation (new BinaryOperators.equals) $1 $3 }
  | expression "!=" expression { new Expressions.binaryOperation (new BinaryOperators.notEquals) $1 $3 }
  | expression "<" expression  { new Expressions.binaryOperation (new BinaryOperators.lessThan) $1 $3 }
  | expression "<=" expression { new Expressions.binaryOperation (new BinaryOperators.lessThanEquals) $1 $3 }
  | expression ">" expression  { new Expressions.binaryOperation (new BinaryOperators.greaterThan) $1 $3 }
  | expression ">=" expression { new Expressions.binaryOperation (new BinaryOperators.greaterThanEquals) $1 $3 }
  | expression "&&" expression { new Expressions.binaryOperation (new BinaryOperators.logicalAnd) $1 $3 }
  | expression "||" expression { new Expressions.binaryOperation (new BinaryOperators.logicalOr) $1 $3 }