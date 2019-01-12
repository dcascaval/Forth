
open Interpret
open Core

(* No command line args quite yet. *)
let () =  
  let state = Parse.parse_file "test/test.forth" in 
  let _ = Interpret.evaluate state.program state.defns in 
  ()