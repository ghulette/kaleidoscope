open Printf
open Util
  
let error msg = failwith msg

let parse chn = 
  try 
    let parser = Parser.main Lexer.token in
    parser (Lexing.from_channel chn)
  with 
    | Lexer.Illegal_char t -> 
      error (sprintf "Syntax error: illegal character %c" t)
    | Parsing.Parse_error -> 
      error "Syntax error: parsing failed"
    | Failure _ -> 
      error "Unknown error"

let main () =
  let ee = Codegen.init_jit () in
  let funcs = parse stdin in
  List.iter (fun f -> ignore (Codegen.codegen_func f)) funcs;
  banner "LLVM IR";
  Llvm.dump_module Codegen.the_module;
  let kal_main = begin
    match Llvm.lookup_function "_main" Codegen.the_module with
      | Some f -> f
      | None -> error "Main not found"
  end in
  banner "Output";
  Codegen.run_jit kal_main ee

;;

main ()
