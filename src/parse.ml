
open Core
module S = String.Map

let open_file f = In_channel.with_file f ~f:In_channel.input_lines 

(* Strip whitespace and remove empties in prep for parsing *)
let clean_strings strings = strings
    |>  List.map ~f:(String.strip)
    |>  List.filter ~f:(fun s -> s <> "")

let drop_including elem list =  
  (list |> List.drop_while ~f:(fun e -> e <> elem)
        |> List.drop) 1

type parse_state = {
  tokens  : string list; (* Unparsed *)
  program : Interpret.program; (* Parsed *)
  defns   : (Interpret.program) String.Map.t (* Words *)
}

 (* We attempt to parse a number. Sometimes we fail. *)
  let parse_num num = 
    match int_of_string_opt num with
      | Some n -> Interpret.Value n
      | _ -> failwith (sprintf "Syntax error: unexpected token \"%s\"" num)
  
  (* Comments get ignored entirely. *)
  let parse_comment = drop_including ")" 
  let parse_dot = drop_including ";" 

  (* Parses a word definition *)
  let rec parse_defn state =  
    let defn_tokens = List.take_while ~f:(fun s -> s <> ";") state.tokens in 
    let rest_tokens = drop_including ";" state.tokens in
    match defn_tokens with 
    | ":" :: name :: subprogram -> 
       let substate = parse {state with tokens = subprogram; program = []} in
       let table = S.update substate.defns name ~f:(function _ -> List.rev substate.program) in 
       { state with tokens = rest_tokens; defns = table }
    | _ -> failwith "Empty definition (: ;)"

  and parse state =
    let open Interpret in
    match state.tokens with 
    | [] -> state
    | token :: xs -> 
      let insert op = { state with tokens = xs; program = Operator op :: state.program } in
      let state' = 
        match token with
        (* Remove source comments *)
        | "(" -> { state with tokens = parse_comment xs } 
        | ":" -> parse_defn state
        (* Dot returns until the next semicolon. *)
        | "." -> { state with tokens = parse_dot xs; 
                   program = Operator DOT :: state.program }

        (* Mathematical operators *)
        | "+" -> insert (Binop ADD) 
        | "-" -> insert (Binop SUB) 
        | "*" -> insert (Binop MUL) 
        | "/" -> insert (Binop DIV) 
        | "%" -> insert (Binop MOD) 

        (* Stack Operators *)
        | "DROP" ->  insert (Stack DROP) 
        | "SWAP" ->  insert (Stack SWAP) 
        | "DUP"  ->  insert (Stack DUP)  
        | "OVER" ->  insert (Stack OVER)
        | "ROT"  ->  insert (Stack ROT)
      
        (* If we're here, it must be a number or a symbol. *)
        (* If it's not in the symbol table, we try to parse it as a number. *)
        | sym ->
          let new_token = 
            if S.mem state.defns sym then Symbol sym 
            else (parse_num sym)
          in 
          { state with tokens = xs; program = new_token :: state.program }
      in 
      parse state'


let parse_line state (tokens : string list) = parse { state with tokens } 
    
(* Open up a forth file and parse it into our interpreter's IR. *)
let parse_file file : parse_state =   
  file |> open_file
       |> List.map ~f:(String.split ~on:' ')
       |> List.map ~f:clean_strings
       |> List.fold ~init:{ tokens = []; program = []; defns = S.empty } ~f:parse_line
       |> function state -> { state with program = List.rev state.program }
