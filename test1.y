%token + - * / ( )
%token id

%%

start : expr
      ;

expr : term expr1
      ;

expr1 : + term expr1
      |
      ;

term : factor term1
      ;

term1 : * factor term1
      |
      ;

factor : id
	| ( expr )
	;

%%
