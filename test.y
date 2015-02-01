%token + - * / ( )
%token id

%%

start : expr ;

expr : term
      | expr + term
      ;

term : factor
      | term * factor
      ;


factor : id
	| ( expr )
	;

%%
