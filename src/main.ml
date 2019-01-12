
open Interpret
open Core

(* No command line args quite yet. *)
let () =  
  let open Parse in 
  let state = parse_file "test/test.forth" in 
  Interpret.evaluate state.program state.defns