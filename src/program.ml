
open Core
module S = String.Map

(*** Type definitions & printing functions ***)

(* Mathematical Operators *)
type binop = ADD | SUB | MUL | DIV | MOD | LT | GT | EQ | AND | OR | INVERT

(* Data-based operations *)
type stackop = DUP | SWAP | DROP | OVER | ROT

(* I/O (really just o, right now) *)
type output = DOT | EMIT | NEWLINE

(* Different conceptual types of operators in a program. Conditionals and loops 
 * contain subprograms but are modelled as a single node. *)
type operator = 
  | Binop of binop                                    (* Math *)
  | Stack of stackop                                  (* Data (stack manipulation) *)
  | Outop of output                                   (* Output operators *)
  | Condition of token list * token list option       (* Conditional statement *)
  | Loop of token list                                (* Loop statement *)

(* Different types of interpretable tokens. Values are restricted to numbers. 
 * Symbols are expanded into their defined subprograms. *)
and token = 
  | Value of int32 
  | Operator of operator 
  | Symbol of string
  | INDEX 

(* Two stacks: a program and data stack. *)
type program = token list 
type stack = int32 list 

(**  Mapping from keywords (strings) to programs (lists of interpretable tokens) *)
type definition_map = (program) S.t

(*** Printing functions, for convenience. ***)

let pp_binop = 
  function | ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/" | MOD -> "%" 
           | AND -> "&" | OR  -> "|" | LT  -> "<" | GT  -> ">" | EQ  -> "="
           | INVERT -> "INVERT" 

let pp_stackop = 
  function DUP -> "DUP" | SWAP -> "SWAP" | DROP -> "DROP" | OVER -> "OVER" | ROT -> "ROT"

let pp_output = 
  function DOT -> "." | EMIT -> "EMIT" | NEWLINE -> "CR"

let rec pp_condition c1 c2 =  
  match c2 with 
  | Some t -> sprintf "IF %s ELSE %s THEN" (pp_program c1) (pp_program t)
  | None   -> sprintf "IF %s THEN" (pp_program c1)

and pp_loop toks = sprintf "DO %s LOOP" (pp_program toks)

and pp_op = function 
  | Binop b -> pp_binop b 
  | Stack s -> pp_stackop s
  | Outop o -> pp_output o
  | Condition (c1,c2) -> pp_condition c1 c2
  | Loop tokens -> pp_loop tokens

and pp_token = function 
  | Value v -> Int32.to_string v 
  | Operator o -> pp_op o
  | Symbol s -> s
  | INDEX -> "i"

and pp_program tokens = "[" ^ String.concat ~sep:"," (List.map ~f:pp_token tokens) ^ "]"

let pp_data data = "[" ^ String.concat ~sep:"," (List.map ~f:Int32.to_string data) ^ "]"

let invalid_op op = 
  failwith (sprintf "program doesn't have enough tokens to perform operation (%s)" (pp_op op))