
open Interpret
open Core

(* Evaluate a file. *)
let file_evaluator filename = 
  let open Parse in 
  let state = parse_file filename in 
  Interpret.evaluate state.program state.keywords

(* Command-line argument *)
let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Interprets a forth program in a file."
    [%map_open
      let filename = anon ("filename" %: string) in
      fun () -> file_evaluator filename ]

let () = 
  Command.run command 