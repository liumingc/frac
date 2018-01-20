%{
open Helper
open Printf
%}

%token <string> NAME
%token <int> NUM
%token LET IN
%token EQ
%token IF THEN ELSE
%token PLUS MINUS MULT DIV MOD
%token EOF
%token AND
%token SEMICOLON

%left AND
%right EQ
%left PLUS MINUS
%left MULT DIV MOD
/* %token ASSIGN */

%type <Helper.exp list> prog
%start prog

%%

prog:
  expr EOF
  {
    [$1]
  }
  ;

expr :
  LET bindings IN expr
  {
    Let($2, $4)
  }
| IF expr THEN expr ELSE expr
  {
    If($2, $4, $6)
  }
| expr binop expr
  {
    Binop($1, $2, $3)
  }
| NUM
  {
    Const($1)
  }
| NAME
  {
    Var($1)
  }
;

binop :
  PLUS  {Add}
| MINUS {Sub}
| MULT  {Mul}
| DIV   {Div}
;

bindings :
  NAME EQ expr
  {
    [Bind($1, $3)]
  }
| bindings AND bindings
  {
    $1 @ $3
  }

%%


