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
    begin match op with
      | Ast.Add -> build_add lhs_val rhs_val "addtmp" builder
      | Ast.Sub -> build_sub lhs_val rhs_val "subtmp" builder
      | Ast.Mult -> build_mul lhs_val rhs_val "multmp" builder
      | Ast.Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      | Ast.CompLT ->
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

let codegen_proto = function
  | Ast.Prototype (name,args) ->
    let type_sig = Array.make (List.length args) (double_type context) in
    let ft = function_type (double_type context) type_sig in
    let f = match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some f ->
        (* If 'f' already has a body, reject this. *)
        if Array.length (basic_blocks f) == 0 then () else
          raise (Error "redefinition of function");
        (* If 'f' took a different number of arguments, reject. *)
        if Array.length (params f) == List.length args then () else
          raise (Error "redefinition of function with different # args");
        f in
    Array.iteri begin fun i a ->
      let arg_name = List.nth args i in
      set_value_name arg_name a;
      Hashtbl.add named_values arg_name a;
    end (params f);
    f

let codegen_func = function
  | Ast.Extern proto -> failwith "Extern not supported"
  | Ast.Function (proto,body) ->
    Hashtbl.clear named_values;
    let the_function = codegen_proto proto in
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    try
      let ret_val = codegen_expr (List.hd body) in
      let _ = build_ret ret_val builder in
      Llvm_analysis.assert_valid_function the_function;
      the_function
    with e ->
      delete_function the_function;
      raise e
