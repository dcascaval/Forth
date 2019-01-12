
open Core
module S = String.Map

let say = prerr_endline

(*** Type definitions ***)

(* Mathematical Operators *)
type binop = ADD | SUB | MUL | DIV | MOD | LT | GT | EQ | AND | OR | INVERT

(* Data-based operations *)
type stackop = DUP | SWAP | DROP | OVER | ROT

(* I/O (really just o, right now) *)
type output = DOT | EMIT | NEWLINE

type operator = Binop of binop | Stack of stackop | Outop of output
type token = Value of int32 | Operator of operator | Symbol of string

(* Two stacks: a program and data stack. *)
type program = token list 
type stack = int32 list 


(*** Printing functions, for convenience. ***)
let pp_binop = 
  function | ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/" | MOD -> "%" 
           | AND -> "&" | OR  -> "|" | LT  -> "<" | GT  -> ">" | EQ  -> "="
           | INVERT -> "invert" 

let pp_stackop = 
  function DUP -> "DUP" | SWAP -> "SWAP" | DROP -> "DROP" | OVER -> "OVER" | ROT -> "ROT"

let pp_output = 
  function DOT -> "." | EMIT -> "EMIT" | NEWLINE -> "CR"

let pp_op = function 
  | Binop b -> pp_binop b 
  | Stack s -> pp_stackop s
  | Outop o -> pp_output o

let pp_token = function 
  | Value v -> Int32.to_string v 
  | Operator o -> pp_op o
  | Symbol s -> s

let pp_program tokens = "[" ^ String.concat ~sep:"," (List.map ~f:pp_token tokens) ^ "]"
let pp_data data = "[" ^ String.concat ~sep:"," (List.map ~f:string_of_int data) ^ "]"

let invalid_op op = 
  failwith (sprintf "program doesn't have enough tokens to perform operation (%s)" (pp_op op))


(*** Actual interpreter! ***)

(* Interpret binop (perform simple computation on what must be numbers) *)
let operate_binop stack binop =
  match binop, stack with
  | INVERT, a :: xs -> (Int32.bit_not a) :: xs
  | _ , a :: b :: xs -> (* Valid operation *)
    let result = (match binop with
     | ADD -> [Int32.(a + b)]
     | SUB -> [Int32.(a - b)]
     | MUL -> [Int32.(a * b)]
     | DIV -> [Int32.(a / b)]
     | MOD -> [Int32.rem a b]
     | AND -> [Int32.bit_and a b]
     | OR ->  [Int32.bit_or  a b]
     | EQ  -> [if a = b then -1l else 0l]
     | LT  -> [if a < b then -1l else 0l]
     | GT  -> [if a > b then -1l else 0l]
     | INVERT -> failwith "Invalid inversion. (Unreachable)"
    )
    in result @ xs 

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
  | (DOT,  x :: xs) -> printf "%s " (Int32.to_string x); xs
  | (EMIT, x :: xs) -> printf "%s" (x |> Int32.to_int_exn |> Char.of_int_exn |> Char.to_string); xs
  | (NEWLINE, _)    -> printf "\n"; data
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
    | Outop output -> (operate_output data output, program)

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
