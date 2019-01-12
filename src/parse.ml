
open Core
module S = String.Map

type parse_state = {
  tokens  : string list; (* Unparsed *)
  program : Interpret.program; (* Parsed *)
  defns   : (Interpret.program) String.Map.t (* Words *)
}

let say = prerr_endline

let print_tokens toks = 
  "[" ^ String.concat ~sep:", " toks ^ "]"


let open_file f = In_channel.with_file f ~f:In_channel.input_lines 

(* Strip whitespace and remove empties in prep for parsing *)
let clean_strings strings = strings
    |>  List.map ~f:(String.strip)
    |>  List.filter ~f:(fun s -> s <> "")

let drop_including elem list =  
  (list |> List.drop_while ~f:(fun e -> e <> elem)
        |> List.drop) 1

let remove_comments program =
  let (_,res) = List.fold program ~init:(false,[]) 
  ~f:(fun (drop,ls) e -> 
    match e with 
    | "(" -> (true,ls)
    | ")" -> (false,ls)
    | _ -> let ls' = if drop then ls else e::ls in (drop,ls')) in
    List.rev res

 (* We attempt to parse a number. Sometimes we fail. *)
  let parse_num num = 
    match int_of_string_opt num with
      | Some n -> Interpret.Value (Int32.of_int_exn n)
      | _ -> failwith (sprintf "Syntax error: unexpected token %s" num)

  (* Parses a word definition *)
  let rec parse_defn state =  
    let defn_tokens = List.take_while ~f:(fun s -> s <> ";") state.tokens in 
    let rest_tokens = drop_including ";" state.tokens in
    match defn_tokens with 
    | ":" :: name :: subprogram -> 
       let substate = parse {state with tokens = subprogram; program = []} in
       let table = S.update substate.defns name ~f:(function _ -> List.rev substate.program) in 
       (* say (sprintf "Adding %s : %s" name (Interpret.pp_program (S.find_exn table name))); *)
       { state with tokens = rest_tokens; defns = table }
    | _ -> failwith "Empty definition (: ;)"
  
  and parse_conditional state = 
    let open Interpret in
    match state.tokens with 
    | "IF" :: s1 :: "THEN" :: xs -> (* Simple if *)
      let s1 = parse_token state s1 in
      let op = Condition (s1,None) in
      { state with tokens = xs; program = Operator op :: state.program }
      (* If with a condition *)
    | "IF" :: s1 :: "ELSE" :: s2 :: "THEN" :: xs -> 
      let s1,s2 = parse_token state s1, parse_token state s2 in
      let op = Condition (s1,Some s2) in
      { state with tokens = xs; program = Operator op :: state.program }
    | _ -> failwith (sprintf "Invalid Conditional: (%s)" (String.concat state.tokens))

  and parse_token state token = 
    let open Interpret in 
    let insert op = Operator op in 
    match token with
      (* Output operators *)
      | "." ->    insert (Outop DOT)
      | "EMIT" -> insert (Outop EMIT)
      | "CR" ->   insert (Outop NEWLINE)

      (* Mathematical operators *)
      | "+" -> insert (Binop ADD) 
      | "-" -> insert (Binop SUB) 
      | "*" -> insert (Binop MUL) 
      | "/" -> insert (Binop DIV) 
      | "%" -> insert (Binop MOD) 
      | "&" -> insert (Binop AND)
      | "|" -> insert (Binop OR)
      | "=" -> insert (Binop EQ)
      | "<" -> insert (Binop LT)
      | ">" -> insert (Binop GT)
      | "invert" -> insert (Binop INVERT)

      (* Stack Operators *)
      | "DROP" ->  insert (Stack DROP) 
      | "SWAP" ->  insert (Stack SWAP) 
      | "DUP"  ->  insert (Stack DUP)  
      | "OVER" ->  insert (Stack OVER)
      | "ROT"  ->  insert (Stack ROT)
    
      (* If we're here, it must be a number or a symbol. *)
      (* If it's not in the symbol table, we try to parse it as a number. *)
      | sym ->
          if S.mem state.defns sym then Symbol sym 
          else (parse_num sym)


  (* Comments have been removed, and the whole program is one continuous stream. *)
  and parse state =
    let open Interpret in
    match state.tokens with 
    | [] -> state
    | token :: xs -> 
      let state' = 
        match token with
        (* Remove source comments *)
        | ":" -> parse_defn state
        | "IF" -> parse_conditional state 
        | _ -> 
          let parsed = parse_token state token in 
          { state with tokens = xs; program = parsed :: state.program }
      in 
      parse state'


let parse_line state (tokens : string list) = 
  (* say (print_tokens tokens); *)
  parse { state with tokens } 
    
(* Open up a forth file and parse it into our interpreter's IR. *)
let parse_file file : parse_state =   
  file |> open_file
       |> List.map ~f:(String.split ~on:' ')
       |> List.map ~f:clean_strings
       |> List.concat 
       |> remove_comments
       |> parse_line { tokens = []; program = []; defns = S.empty }
       |> function state -> { state with program = List.rev state.program }

let parse_input_line state () =
  match In_channel.input_line In_channel.stdin with
  | Some s -> 
    let parsed = s |> String.split ~on:' ' |> clean_strings |> parse_line state in
    Some { parsed with program = List.rev parsed.program }
  | None -> None
