
open Interpret
open Core

(* No command line args quite yet. *)

let repl_evaluator () = 
  let open Parse in 

  let empty = { in_loop = false; tokens = []; program = []; defns = S.empty} in

  let rec repl ?state:(state = empty) () = 
    match parse_input_line state () with 
      | Some st -> 
        (* prerr_endline (Interpret.pp_program st.program); *)
        Interpret.evaluate st.program st.defns;
        repl ~state:st () 
      | None -> ()
  in
  repl ()

let file_evaluator () = 
  let open Parse in 
  let state = parse_file "test/test.forth" in 
  Interpret.evaluate state.program state.defns

let () =  
  file_evaluator ()  
