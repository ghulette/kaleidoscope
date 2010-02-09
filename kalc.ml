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
    match parser (Lexing.from_channel chn) with
      | Some prg -> Left prg
      | None -> Right Unknown_error
  with 
    | Lexer.Illegal_char t -> 
      let err = Syntax_error (sprintf "illegal character %c" t) in
      Right err
    | Parsing.Parse_error -> 
      let err = Syntax_error "parsing failed" in
      Right err
    | Failure x -> Right Unknown_error

let main () =
  match parse stdin with
    | Left prg ->
      let strs = List.map Ast.string_of_expr prg in
      printf "Input\n%s\n" (String.concat "\n" strs);
    | Right err ->
      handle_error err

;;
main ()
