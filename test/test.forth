
( simple operations )
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
