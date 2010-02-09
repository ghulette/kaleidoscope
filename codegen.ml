open Llvm

let codegen_expr = function
  | Ast.Var x -> ()
  | Ast.Number n -> () 
  | Ast.Op (op,e1,e2) -> ()
  | Ast.Call (f,args) -> ()
