
open Core
module S = String.Map

let say = prerr_endline

(*** Type definitions ***)

(* Mathematical Operators *)
type binop = ADD | SUB | MUL | DIV | MOD  

(* Data-based operations *)
type stackop = DUP | SWAP | DROP | OVER | ROT

(* I/O (really just o, right now) *)
type output = DOT | EMIT | NEWLINE

type operator = Binop of binop | Stack of stackop | Outop of output
type token = Value of int | Operator of operator | Symbol of string

(* Two stacks: a program and data stack. *)
type program = token list 
type stack = int list 


(*** Printing functions, for convenience. ***)
let pp_binop = 
  function ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/" | MOD -> "%"

let pp_stackop = 
  function DUP -> "DUP" | SWAP -> "SWAP" | DROP -> "DROP" | OVER -> "OVER" | ROT -> "ROT"

let pp_output = 
  function DOT -> "." | EMIT -> "EMIT" | NEWLINE -> "CR"

let pp_op = function 
  | Binop b -> pp_binop b 
  | Stack s -> pp_stackop s
  | Outop o -> pp_output o

let pp_token = function 
  | Value v -> string_of_int v 
  | Operator o -> pp_op o
  | Symbol s -> s

let pp_program tokens = "[" ^ String.concat ~sep:"," (List.map ~f:pp_token tokens) ^ "]"
let pp_data data = "[" ^ String.concat ~sep:"," (List.map ~f:string_of_int data) ^ "]"

let invalid_op op = 
  failwith (sprintf "program doesn't have enough tokens to perform operation (%s)" (pp_op op))


(*** Actual interpreter! ***)

(* Interpret binop (perform simple computation on what must be numbers) *)
let operate_binop stack binop =
  match stack with
  | a :: b :: xs -> (* Valid operation *)
    let result = (match binop with
     | ADD -> (a + b)
     | SUB -> (a - b)
     | MUL -> (a * b)
     | DIV -> (a / b)
     | MOD -> (a mod b))
    in result :: xs 
  | _ -> invalid_op (Binop binop)

(* Interpret stack operation (modify or swap possibly multiple elements, generic) *)
let operate_stackop stack stackop =
  match stackop, stack with  
  | (DUP,  x::xs)       -> x :: x :: xs 
  | (SWAP, x::y::xs)    -> y :: x :: xs 
  | (DROP, x::xs)       -> xs
  | (OVER, x::y::xs)    -> x :: y :: x :: xs 
  | (ROT,  x::y::z::xs) -> y :: z :: x :: xs
  | _ -> invalid_op (Stack stackop)

let operate_output data output =  
  match output, data with 
  | (DOT,  x :: xs) -> printf "%d " x
  | (EMIT, x :: xs) -> printf "%s" (Char.to_string (Char.of_int_exn x))
  | (NEWLINE, _)    -> printf "\n"
  | _ -> invalid_op (Outop output)

(* Dispatch action of the interpreter *)
let operate defns (data, program) token = 
  match token with
  | Value v -> (v :: data, program)
  | Symbol s -> (data, S.find_exn defns s @ program) 
  | Operator op -> 
    match op with 
    | Binop binop -> (operate_binop data binop, program)
    | Stack stackop -> (operate_stackop data stackop, program)
    | Outop output -> operate_output data output; (data, program)

(* Main loop. *)
let evaluate program definitions =
  let operate = operate definitions in
  let rec eval (data,prog) = 
    match prog with 
    | [] -> ()
    | token :: prog' ->
      let (data',prog'') = operate (data,prog') token in
      eval (data',prog'')
  in
     eval ([],program);
     printf ("ok\n");
