
open Interpret
open Core

(* No command line args quite yet. *)
let () =  
  let program = Parse.parse_file "test/test.forth" in 
  let _ = Interpret.evaluate program in 
  ()