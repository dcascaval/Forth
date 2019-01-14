
open Core
module S = String.Map

type token = string 
type token_stream = string list

(* Main abstraction in this mini-parser. This represents an in-progress part 
 * of parsing. We maintain tokens as our current parse stream state, program 
 * as our so-far-parsed progam (reversed), and definitions as a mapping from 
 * defined keywords to their substitution programs. *)
type parse_state = {
  tokens  : token_stream; (* Unparsed strings *)
  program : Program.program; (* Parsed strings *)
  defns   : (Program.program) String.Map.t; (* New word definitions *)
  in_loop : bool; (* State variable, assigns special meaning to index *)
}

(* Information returned from parser to main program. *)
type parse_result = { 
  program : Program.program; (* Program to Program. *)
  keywords: (Program.program) String.Map.t  (* Finalized word definitions. *)
}

(**************  Helper Functions ***************)

let print_tokens toks =  "[" ^ String.concat ~sep:", " toks ^ "]"

(* Wrapper to open file and split on newline *)
let open_file f = In_channel.with_file f ~f:In_channel.input_lines 

(* Strip whitespace and remove empties in prep for parsing *)
let clean_strings strings = strings
    |>  List.map ~f:(String.strip)
    |>  List.filter ~f:(fun s -> s <> "")

let drop_including elem list =  
  (list |> List.drop_while ~f:(fun e -> e <> elem)
        |> List.drop) 1

(* Splits a list on the next occurrence of an element. *)
let split_on_elem elem list =
  let (hd,tl) = List.split_while ~f:(fun x -> x <> elem) list in 
  (hd,List.drop tl 1)

(* Finds which token in a set occurs next in a list. *)
let next_token_in_set list set =  
  List.find list ~f:(fun e -> List.mem ~equal:String.equal set e)

(* Parses a token stream to remove all comments and delimiting characters. *)
let remove_comments (program : token_stream) : token_stream =
  let (_,res) = List.fold program ~init:(0,[]) 
  ~f:(fun (drop,ls) e -> 
    match e with 
    | "(" -> (drop + 1,ls)
    | ")" -> ((if drop > 0 then drop - 1 else drop),ls)
    | _ -> let ls' = if drop > 0 then ls else e::ls in (drop,ls')) in
    List.rev res


(**************  Main Parser Functions ***************)

 (* Parse a number, if we can. *)
  let parse_num num =
    match int_of_string_opt num with
      | Some n -> Some (Program.Value (Int32.of_int_exn n))
      | None -> None

  (* Parse a word definition. Collects all tokens up to the semicolon following 
   * the current location in the stream, and updates the `defns` entry in the 
   * returned parse state *)
  let rec parse_defn state =  
    let (defn_tokens, rest) = split_on_elem ";" state.tokens in
    match defn_tokens with 
    | ":" :: name :: subprogram -> 
       let substate = parse {state with tokens = subprogram; program = []} in
       let table = S.update substate.defns name ~f:(function _ -> List.rev substate.program) in 
       { state with tokens = rest; defns = table }
    | _ -> failwith "Empty definition (: ;)"
  
  (* Parse a conditional (IF ?ELSE THEN) statement. Detects if an ELSE is present before
   * the next THEN in the stream in order to determine whether to include an 'else' clause
   * in the conditional token. If no following 'THEN' is found, an error is thrown. *)
  and parse_conditional state = 
    let open Program in
    match next_token_in_set (state.tokens) ["THEN";"ELSE"] with 
    | Some "THEN" -> 
      let (cond,rest) = split_on_elem "THEN" state.tokens in 
      let s1 = parse { state with tokens = cond; program = [] } in 
      let op = Condition (List.rev s1.program,None) in
      { state with tokens = rest; program = Operator op :: state.program }
    | Some "ELSE" -> 
      let (cond,rest) = split_on_elem "THEN" state.tokens in 
      let (c1,c2) = split_on_elem "ELSE" cond in 
      let s1 = parse { state with tokens = c1; program = [] } in
      let s2 = parse { state with tokens = c2; program = [] } in
      let op = Condition (List.rev s1.program,Some (List.rev s2.program)) in
      { state with tokens = rest; program = Operator op :: state.program }
    | _ -> failwith (sprintf "Unfinished Conditional: (%s)" (String.concat ~sep:" " state.tokens))

 (* Parse a loop definition (do-loops as described at:
     https://skilldrick.github.io/easyforth/#conditionals-and-loops
    This allows a simple incrementing loop from two range bounds and creates "i" as a special 
    token character within the loop body that represens the index. Critically, this does not 
    extend to nested loops, and the inner's loop's index will take precedence. *)
  and parse_loop state =
    let open Program in
    let loop_tokens = List.take_while ~f:(fun s -> s <> "LOOP") state.tokens in 
    let rest_tokens = drop_including "LOOP" state.tokens in
    let parsed = parse { state with in_loop = true; tokens = loop_tokens; program = [] } in
    { state with tokens = rest_tokens; program = Operator (Loop (List.rev parsed.program)) :: state.program }

  (** Parse a single token. Requires current parse_state for definition access. *)
  and parse_token state (token : token) = 
    let open Program in 
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
      | "INVERT" -> insert (Binop INVERT)

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
          else match parse_num sym with
           | Some parsed -> parsed
           | None -> failwith (sprintf "Syntax error: unexpected token %s" sym)


  (* Comments have been removed, and the whole program is one continuous stream. *)
  and parse state =
    let open Program in
    match state.tokens with 
    | [] -> state
    | token :: xs -> 
      let state' = 
        match token with
        (* Remove source comments *)
        | ":" -> parse_defn state
        | "IF" -> parse_conditional { state with tokens = xs }
        | "DO" -> parse_loop { state with tokens = xs }
        | "i" -> 
          let parsed = if state.in_loop then INDEX else parse_token state token in 
          { state with tokens = xs; program = parsed :: state.program }
        | _ -> 
          let parsed = parse_token state token in 
          { state with tokens = xs; program = parsed :: state.program }
      in 
      parse state'


let parse_line state (tokens : string list) =  parse { state with tokens } 
    
(** Main loop. Opens up a file and returns a completed parse_state **)
let parse_file file : parse_result =   
  file |> open_file
       |> List.map ~f:(String.split ~on:' ')
       |> List.map ~f:clean_strings
       |> List.concat 
       |> remove_comments
       |> parse_line { tokens = []; program = []; defns = S.empty; in_loop = false;  }
       |> function (state : parse_state) ->
          { program = List.rev state.program; keywords = state.defns }

(* Parses a single line of input with a parse_state and returns the updated state. Useful for REPL. *)
let parse_input_line state () =
  match In_channel.input_line In_channel.stdin with
  | Some s -> 
    let parsed = s |> String.split ~on:' ' |> clean_strings |> parse_line state in
    Some { parsed with program = List.rev parsed.program }
  | None -> None
