open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let rec codegen_expr = function
  | Ast.Var x -> Hashtbl.find named_values x
  | Ast.Number n -> const_float (double_type context) n
  | Ast.Op (op,lhs,rhs) ->  
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin 
    match op with
      | Ast.Add -> build_add lhs_val rhs_val "addtmp" builder
      | Ast.Sub -> build_sub lhs_val rhs_val "subtmp" builder
      | Ast.Mult -> build_mul lhs_val rhs_val "multmp" builder
      | Ast.Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      (* 
      | '<' ->
        (* Convert bool 0/1 to double 0.0 or 1.0 *)
        let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
        build_uitofp i double_type "booltmp" builder
      *)
    end
  | Ast.Call (f,args) -> raise (Error "call unsupported")
  
    (* | Ast.Call (callee, args) ->
        (* Look up the name in the module table. *)
        let callee =
          match lookup_function callee the_module with
          | Some callee -> callee
          | None -> raise (Error "unknown function referenced")
        in
        let params = params callee in

        (* If argument mismatch error. *)
        if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
        let args = Array.map codegen_expr args in
        build_call callee args "calltmp" builder *)
