{
open Parser

let line_count = ref 1
exception Illegal_char of char
}

let id = ['a'-'z' 'A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let num = ['1' - '9'](['0'-'9']*)(('.'['0'-'9']+)?)

rule tokenizer = parse    
  | "extern"   { EXTERN }
  | "def"      { DEF }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | ','        { COMMA }
  | ';'        { SEMI }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES  }
  | '/'        { DIV }
  | '<'        { LT }
  | id as word { ID word }
  | num as n   { NUMBER (float_of_string n) }
  | [' ' '\t'] { tokenizer lexbuf }
  | ['\n']     { incr line_count; tokenizer lexbuf }
  | _ as c     { raise (Illegal_char c) }
  | eof        { EOF }
