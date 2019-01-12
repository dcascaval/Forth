
open Core

let open_file f = In_channel.with_file f ~f:In_channel.input_lines 

(* Strip whitespace and remove empties in prep for parsing *)
let clean_strings strings = strings
    |>  List.map ~f:(String.strip)
    |>  List.filter ~f:(fun s -> s <> "")


(* Handwritten parsing. *)
let parse_line (strings : string list) : Interpret.token list =
  let open Interpret in

  (* We attempt to parse a number. Sometimes we fail. *)
  let parse_num num = 
    match int_of_string_opt num with
      | Some n -> Value n
      | _ -> failwith (sprintf "Syntax error: unexpected token %s" num)
  in

  (* Comments get ignored entirely. *)
  let parse_comment program = 
    let xs' = (List.drop_while ~f:(fun s -> s <> ")") program) in 
    match xs' with 
    | [] -> [] 
    | x :: program' -> program'
  in

  let rec parse strings acc =
    match strings with 
    | [] -> acc
    | str :: xs -> 
      let rest,acc' =
      match str with
        (* Remove source comments *)
        | "(" -> parse_comment xs, acc
        
        (* Dot ends the program. *)
        | "." -> [],(Operator DOT :: acc)

        (* Mathematical operators *)
        | "+" -> xs, Operator (Binop ADD) :: acc
        | "-" -> xs, Operator (Binop SUB) :: acc
        | "*" -> xs, Operator (Binop MUL) :: acc
        | "/" -> xs, Operator (Binop DIV) :: acc
        | "%" -> xs, Operator (Binop MOD) :: acc

        (* Stack Operators *)
        | "DROP" -> xs, Operator (Stack DROP) :: acc
        | "SWAP" -> xs, Operator (Stack SWAP) :: acc
        | "DUP"  -> xs, Operator (Stack DUP)  :: acc
      
        (* If it's here, it must be a number or a symbol.
         * Right now we only support numbers. *)
        | num -> xs, parse_num num :: acc 
      in 
       parse rest acc' 
    in 
    List.rev (parse strings [])


(* Open up a forth file and parse it into our interpreter's IR. *)
let parse_file file : Interpret.program =   
  file |> open_file
       |> List.map ~f:(String.split ~on:' ')
       |> List.map ~f:clean_strings
       |> List.map ~f:parse_line
       |> List.concat

(* Parse a line of forth all on its own. *)
let parse_string string : Interpret.program = []