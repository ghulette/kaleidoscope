%{  
%}

%token <string> ID, 
%token <float> NUMBER
%token PLUS, MINUS, TIMES, DIV
%token LPAREN, RPAREN, COMMA, SEMI
%token DEF, EXTERN
%token EOF
%left PLUS, MINUS
%left TIMES, DIV
%start main
%type <(Ast.expr list option)> main

%%

main:
  | stmts EOF { Some $1 }
  | { None }
;

stmts:
  | stmt SEMI stmts { [$1] @ $3 }
  | { [] }

stmt:
  | expr { $1 }
  | def { $1 }
  | extern { $1 }
;

proto: ID LPAREN id_list RPAREN { Ast.Prototype ($1,$3) };

def: DEF proto expr { Ast.Var "def" };

extern: EXTERN proto { Ast.Var "proto" };

expr:
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN expr_list RPAREN { Ast.Call ($1,$3) }
  | ID { Ast.Var $1 }
  | NUMBER { Ast.Number $1 }
  | bin_expr { $1 }
;

bin_expr:
  | expr PLUS expr { Ast.Op (Ast.Add,$1,$3) }
  | expr MINUS expr { Ast.Op (Ast.Sub,$1,$3) }
  | expr TIMES expr { Ast.Op (Ast.Mult,$1,$3) }
  | expr DIV expr { Ast.Op (Ast.Div,$1,$3) }
;

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { [$1] @ $3 }
;

id_list:
  | ID { [$1] }
  | ID COMMA id_list { [$1] @ $3 }
;
