%{
open Printf

let sym_tbl = Hashtbl.create 10;;

let add_def proto exprs =
  let key = Ast.prototype_name proto in
  let value = (proto,exprs) in
  Hashtbl.add sym_tbl key value
;;

%}

%token <string> ID, 
%token <float> NUMBER
%token PLUS, MINUS, TIMES, DIV, LT
%token LPAREN, RPAREN, COMMA, SEMI
%token DEF, EXTERN
%token EOF
%left LT
%left PLUS, MINUS
%left TIMES, DIV
%start main
%type <((string,(Ast.proto * Ast.expr list)) Hashtbl.t)> main

%%

main: stmts EOF { 
  let main_proto = Ast.Prototype ("_main",[]) in
  add_def main_proto $1;
  sym_tbl
}

stmts:
  | stmt SEMI stmts { $1 @ $3 }
  | { [] }

stmt:
  | expr { [$1] }
  | def { [] }

proto: ID LPAREN id_list RPAREN { Ast.Prototype ($1,$3) }

def: DEF proto expr { add_def $2 [$3]; [] }

expr:
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN expr_list RPAREN { Ast.Call ($1,$3) }
  | ID { Ast.Var $1 }
  | NUMBER { Ast.Number $1 }
  | bin_expr { $1 }

bin_expr:
  | expr PLUS expr { Ast.Op (Ast.Add,$1,$3) }
  | expr MINUS expr { Ast.Op (Ast.Sub,$1,$3) }
  | expr TIMES expr { Ast.Op (Ast.Mult,$1,$3) }
  | expr DIV expr { Ast.Op (Ast.Div,$1,$3) }
  | expr LT expr { Ast.Op (Ast.CmpLT,$1,$3) }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { [$1] @ $3 }

id_list:
  | ID { [$1] }
  | ID COMMA id_list { [$1] @ $3 }
