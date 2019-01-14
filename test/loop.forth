( loops )
: loop_test 10 0 DO i . CR LOOP ; 
CR loop_test ( 0 1 2 3 ... etc, in new lines. )

( loops have exclusive upper bounds )
: add_loop 101 0 DO i + LOOP ;
0 add_loop . CR ( 5050 )

( Conditionals and indices in loops )
: alt_loop 100 0 DO i 2 % IF i + ELSE i - THEN ; 
0 alt_loop . CR  ( 50 )