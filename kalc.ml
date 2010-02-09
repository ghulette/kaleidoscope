open Printf
open Util

type little_error = 
  | Syntax_error of string
  | Unknown_error
  
let err_to_string = function
  | Syntax_error msg -> sprintf "Syntax error: %s" msg
  | Unknown_error -> "Unknown error"
  
let handle_error err =
  let msg = err_to_string err in
  printf "%s\n" msg;
  exit 1

let parse chn = 
  try 
    let parser = Parser.main Lexer.tokenizer in
    let sym_tbl = parser (Lexing.from_channel chn) in
    Left sym_tbl
  with 
    | Lexer.Illegal_char t -> 
      let err = Syntax_error (sprintf "illegal character %c" t) in
      Right err
    | Parsing.Parse_error -> 
      let err = Syntax_error "parsing failed" in
      Right err
    | Failure x -> Right Unknown_error
    
let dump _ (proto,exprs) =
  let dump_expr = fun e -> "\t" ^ (Ast.string_of_expr e) ^ ";" in
  let expr_strs = List.map dump_expr exprs in
  let expr_str = String.concat "\n" expr_strs in
  printf "Function: %s\n" (Ast.string_of_proto proto);
  printf "%s\n" expr_str

let main () =
  match parse stdin with
    | Left sym_tbl ->
      Hashtbl.iter dump sym_tbl
    | Right err ->
      handle_error err

;;
main ()
