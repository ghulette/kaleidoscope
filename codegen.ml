open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "Kaleidoscope"
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
      | Ast.CmpLT ->
        (* Convert bool 0/1 to double 0.0 or 1.0 *)
        let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
        build_uitofp i (double_type context) "booltmp" builder
    end
  | Ast.Call (func_id,args) ->
    let f = match lookup_function func_id the_module with
      | Some f -> f
      | None -> raise (Error "unknown function referenced")
    in
      let params = params f in
      (* If argument mismatch error. *)
      if Array.length params == List.length args then () else
        raise (Error "incorrect # arguments passed");
      let args = List.map codegen_expr args in
      let args_array = Array.of_list args in
      build_call f args_array "calltmp" builder
