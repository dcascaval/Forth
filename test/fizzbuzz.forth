( fizzbuzz )
: fizz 90 90 73 70 EMIT EMIT EMIT EMIT ;
: buzz 90 90 85 66 EMIT EMIT EMIT EMIT ; 
: bazz 90 90 65 66 EMIT EMIT EMIT EMIT ; 
: fizz? 3 % 0 = DUP IF fizz THEN ;
: buzz? 5 % 0 = DUP IF buzz THEN ;
: bazz? 7 % 0 = DUP IF bazz THEN ; 
: fizz-buzz? DUP DUP fizz? SWAP buzz? | SWAP bazz? | INVERT ;
: do-fizz-buzz 100 1 DO CR i fizz-buzz? IF i . THEN LOOP ;
do-fizz-buzz CR