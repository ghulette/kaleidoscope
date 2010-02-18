open Printf
open Util
  
let error msg = failwith msg

let parse chn = 
  try 
    let parser = Parser.main Lexer.tokenizer in
    parser (Lexing.from_channel chn)
  with 
    | Lexer.Illegal_char t -> 
      error (sprintf "Syntax error: illegal character %c" t)
    | Parsing.Parse_error -> 
      error "Syntax error: parsing failed"
    | Failure _ -> 
      error "Unknown error"

let main () =
  let funcs = parse stdin in
  List.iter (fun f -> let _ = Codegen.codegen_func f in ()) 
    (List.rev funcs);
  Llvm.dump_module Codegen.the_module

;;

main ()
