
open Core
open Program

(*** Interpreter ***)

(* Interpret binop (perform simple computation on what must be numbers), and put it back on the stack *)
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

(* Interpret I/O operation. Might or might not modify data stack. *)
let operate_output data output =  
  match output, data with 
  | (DOT,  x :: xs) -> printf "%s " (Int32.to_string x); Out_channel.flush Out_channel.stdout; xs
  | (EMIT, x :: xs) -> printf "%s" (x |> Int32.to_int_exn |> Char.of_int_exn |> Char.to_string); Out_channel.flush Out_channel.stdout; xs
  | (NEWLINE, _)    -> printf "\n"; Out_channel.flush Out_channel.stdout; data
  | _ -> invalid_op (Outop output)

(* Interprets a conditional (checks if false, adjusts program stack with according case.) *)
let operate_condition data (if_case,else_case) program = 
  match data with 
  | x :: xs ->
    if x <> 0l then (xs,if_case @ program) else 
    (match else_case with 
    | Some case -> (xs,case @ program)
    | None -> (xs, program))
  | _ -> invalid_op (Condition (if_case,else_case)) 


(* Interpets a loop (replaces all instances of INDEX with a value on each pass). *)
let rec operate_loop defns data body =

  (* recursively replace all of the index tokens. disallows nesting *)
  let rec replace_index_pass idx token = 
    let map_condition = List.map ~f:(replace_index_pass idx) in
    let fail_map = List.map ~f:(function INDEX -> failwith "Can't index nested loop." | x -> x) in 
    match token with 
    | INDEX -> Value idx
    | Operator (Condition (a,None)) -> 
        Operator (Condition (map_condition a,None))
    | Operator (Condition (a,Some b)) -> 
        Operator (Condition (map_condition a, Some (map_condition b)))
    | Operator (Loop toks) ->
        Operator (Loop (fail_map toks))
    | x -> x 
  in

  (* Actual loop operation. *)
  if body = [] then data else (* Don't iterate an empty loop *)
  match data with 
  | start :: finish :: xs ->
    
    (* Tail-recursive loop interpretation function.*)
    let rec loop idx ldata = 
      if idx >= finish then ldata else 
      let body' = List.map ~f:(replace_index_pass idx) body in
      let ldata' = eval defns (ldata,body') in 
      loop Int32.(idx + 1l) ldata'
    in 
    loop start xs

  | _ -> invalid_op (Loop (body)) (* No bounds on stack. *)

(* Dispatch action of the interpreter to the appropriate sub-function  *)
and operate (defns : definition_map) ((data : stack), (program : program)) = 
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

(* Provides a loop that evaluates a program with a given set of definitions in
 * its entirety, and returns the data-stack when it is done. *)
and eval definitions (data,prog) = 
  match prog with 
  | [] -> data
  | _ ->
    let (data',prog') = operate definitions (data,prog) in
    eval definitions (data',prog')

(* Main loop. *)
and evaluate (program : program) (definitions : definition_map) =
  let _ = eval definitions ([],program) in (* Discard data at end of program *)
  printf ("ok\n")
