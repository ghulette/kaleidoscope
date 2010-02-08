%{
%}

%token <string> ID
%token <float> NUMBER
%token DEF
%token EXTERN
%token EOF
%start main
%type <(Ast.expr option)> main

%%

main:
  | expr EOF { Some $1 }
  |          { None }
;

expr:
  | ID { Ast.Var $1 }
  | NUMBER { Ast.Number $1 }
;
