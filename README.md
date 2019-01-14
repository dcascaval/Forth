## Forth Interpreter

Interprets a subset of Forth, including arithmetic, branching, loops, and some basic I/O. 

Some conventions: 
  * All tokens must be strictly space delimited. This includes comment starts, ends, etc. 
  * Interpreter-defined tokens are in all-caps, such as `DROP`,`SWAP`,`IF`... `ELSE` ... `THEN`, etc.
  * Loop indices are denoted as `i` tokens and take on the loop index value at runtime. This forth does not support nested loops. 

To Compile:
  * run `make` on a system that has OCaml 4.05+ and Jane Street Core installed. 

To Run:
  `./finth test/test.forth` 

Supported interpreter-defined tokens:
  * `+`,`-`, `*`, `/`,`%` (Arithmetic)
  * `&`,`|`,`=`,`<`,`>`,`INVERT` (Comparsions and Booleans)
  * `DROP`,`SWAP`,`DUP`,`ROT`,`OVER` (Stack manipulation)
  * `.`,`EMIT`,`CR` (Output)
