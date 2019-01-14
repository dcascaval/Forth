
( simple operations : ( 5,4 ) * )
2 3 + .
11 7 - . 

( defining words )
: square DUP * ; 
3 square .

( branching )
: safedivide DUP 0 = IF DROP ELSE / THEN ;
4 2 safedivide . 
4 0 safedivide .

( loops )
: loop_test 10 0 DO i . CR LOOP ; 
CR loop_test 

( fizzbuzz )
: fizz 90 90 73 70 EMIT EMIT EMIT EMIT ;
: buzz 90 90 85 66 EMIT EMIT EMIT EMIT ; 
: fizz? 3 % 0 = DUP IF fizz THEN ;
: buzz? 5 % 0 = DUP IF buzz THEN ;
: fizz-buzz? DUP fizz? SWAP buzz? | INVERT ;
: do-fizz-buzz 25 1 DO CR i fizz-buzz? IF i . THEN LOOP ;
do-fizz-buzz CR