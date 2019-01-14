
( simple operations )
2 3 + .   ( 5 )
11 7 - .  ( 4 )
150 213 % 11 / 27 * . ( 351 )

( defining words )
: square DUP * ; 
3 square . ( 9 )

: foo 100 + ;  
100 foo foo foo . ( 400 )

( branching )
: safedivide DUP 0 = IF DROP ELSE / THEN ;
4 2 safedivide . ( 2 )
4 0 safedivide . ( 4 )



