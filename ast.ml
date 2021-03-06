type expr =
  | Var of string
  | Number of float
  | Op of binop * expr * expr
  | Call of string * expr list
and binop =
  | Add
  | Sub
  | Mult
  | Div
  | CompLT

type func = 
  | Function of proto * expr list
  | Extern of proto
and proto = Prototype of string * string list

let rec string_of_expr = function
  | Var x -> x
  | Number n -> string_of_float n
  | Op (op,x1,x2) -> 
    let x1s = string_of_expr x1 in
    let x2s = string_of_expr x2 in
    let ops = begin match op with 
      | Add -> "+" 
      | Sub -> "-" 
      | Mult -> "*" 
      | Div -> "/"
      | CompLT -> "<"
    end in 
    "(" ^ x1s ^ ops ^ x2s ^ ")"
  | Call (f,args) -> 
    let arg_strs = List.map string_of_expr args in
    let args_str = String.concat "," arg_strs in
    f ^ "(" ^ args_str ^ ")"

let proto_name = function 
  | Prototype (name,_) -> name

let string_of_proto = function
  | Prototype (f,args) -> 
    let args_str = String.concat "," args in
    f ^ "(" ^ args_str ^ ")"
