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
    printf "Let...\n";
    Let($2, $4)
  }
| IF expr THEN expr ELSE expr
  {
    If($2, $4, $6)
  }
| expr binop expr
  {
    printf "Binop...\n";
    Binop($1, $2, $3)
  }
| NUM
  {
    printf "const -> %d\n" $1;
    Const($1)
  }
| NAME
  {
    printf "var -> %s\n" $1;
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
    printf "Binding name=%s\n" $1;
    [Bind($1, $3)]
  }
| bindings AND bindings
  {
    $1 @ $3
  }

%%


