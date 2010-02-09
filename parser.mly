%{
open Printf  

let binop_of_char = function
  | '+' -> Ast.Add
  | '-' -> Ast.Sub
  | '*' -> Ast.Mult
  | '/' -> Ast.Div
  | x -> invalid_arg (sprintf "No operator %c" x)
  
%}

%token <string> ID
%token <float> NUMBER
%token DEF, EXTERN
%token <char> OP
%token LPAREN, RPAREN
%token COMMA, SEMI
%token EOF
%start main
%type <(Ast.expr list option)> main

%%

main:
  | stmts EOF { Some $1 }
  |           { None }
;

stmts:
  | stmt SEMI stmts { [$1] @ $3 }
  | { [] }

stmt:
  | expr { $1 }
  | def { $1 }
  | proto { $1 }
;

def:
  | DEF ID LPAREN id_list RPAREN expr { Ast.Var "def" }
;

proto:
  | EXTERN ID LPAREN id_list RPAREN { Ast.Var "proto" }
;

expr:
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN expr_list RPAREN { Ast.Call ($1,$3) }
  | ID { Ast.Var $1 }
  | NUMBER { Ast.Number $1 }
  | expr OP expr { Ast.Op ((binop_of_char $2),$1,$3) }
;

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { [$1] @ $3 }
;

id_list:
  | ID { [$1] }
  | ID COMMA id_list { [$1] @ $3 }
;
