
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

type operator = 
  | Binop of binop 
  | Stack of stackop 
  | Outop of output 
  | Condition of token * token option
  | Loop of token list

and token = 
  | Value of int32 
  | Operator of operator 
  | Symbol of string
  | INDEX 

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

let rec pp_condition c1 c2 =  
  match c2 with 
  | Some t -> sprintf "IF %s ELSE %s THEN" (pp_token c1) (pp_token t)
  | None   -> sprintf "IF %s THEN" (pp_token c1)

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


(*** Actual interpreter! ***)

(* Interpret binop (perform simple computation on what must be numbers) *)
let operate_binop stack binop =
  match binop, stack with
  | INVERT, a :: xs -> (Int32.bit_not a) :: xs
  | _ , a :: b :: xs -> (* Valid operation *)
    let result = (match binop with
     | ADD -> Int32.(b + a)
     | SUB -> Int32.(b - a)
     | MUL -> Int32.(b * a)
     | DIV -> Int32.(b / a)
     | MOD -> Int32.rem b a
     | AND -> Int32.bit_and b a
     | OR ->  Int32.bit_or  b a
     | EQ  -> if b = a then -1l else 0l
     | LT  -> if b < a then -1l else 0l
     | GT  -> if b > a then -1l else 0l
     | INVERT -> failwith "Invalid inversion. (Unreachable)"
    )
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
  | (DOT,  x :: xs) -> printf "%s " (Int32.to_string x); Out_channel.flush Out_channel.stdout; xs
  | (EMIT, x :: xs) -> printf "%s" (x |> Int32.to_int_exn |> Char.of_int_exn |> Char.to_string); Out_channel.flush Out_channel.stdout; xs
  | (NEWLINE, _)    -> printf "\n"; Out_channel.flush Out_channel.stdout; data
  | _ -> invalid_op (Outop output)

let operate_condition data (if_case,else_case) program = 
  match data with 
  | x :: xs ->
    if x <> 0l then (xs,if_case :: program)
    else 
    (match else_case with 
    | Some case -> (xs,case :: program)
    | None -> (xs, program))
  | _ -> invalid_op (Condition (if_case,else_case)) 

let rec operate_loop defns data body =
  if body = [] then data else (* Don't iterate an empty loop *)
  match data with 
  | start :: finish :: xs ->
    
    
    (* Tail-recrsive loop *)
    let rec loop idx ldata = 
      if idx >= finish then ldata else 
      let body' = List.map body ~f:(function INDEX -> Value idx | x -> x) in
      let ldata' = eval defns (ldata,body') in 
      loop Int32.(idx + 1l) ldata'
    in 
    loop start xs

  | _ -> invalid_op (Loop (body)) (* No bounds on stack. *)

(* Dispatch action of the interpreter *)
and operate defns (data, program) = 
  match program with
  | [] -> (data, program)
  | INDEX :: program' -> failwith "Index out of loop." 
  | Value v  :: program' -> (v :: data, program')
  | Symbol s :: program' -> (data, S.find_exn defns s @ program') 
  | Operator op :: program' -> 
    match op with 
    | Binop binop -> (operate_binop data binop, program')
    | Stack stackop -> (operate_stackop data stackop, program')
    | Outop output -> (operate_output data output, program')
    | Condition (c1,c2) -> (operate_condition data (c1,c2) program')
    | Loop body -> (operate_loop defns data body, program')

and eval definitions (data,prog) = 
  match prog with 
  | [] -> data
  | _ ->
    let (data',prog') = operate definitions (data,prog) in
    eval definitions (data',prog')

(* Main loop. *)
and evaluate program definitions =
  let _ = eval definitions ([],program) in (* Discard data at end of program *)
  printf ("ok\n");
