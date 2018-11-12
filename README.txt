
SOLUTION:

This program calculates the number of unique configurations for:

 "place (K K Q B R N) on a 6x9 board"
 
to be

  20,136,752

 (see below)


NOTES:

There are 3 areas in the code which may appear to be overly complex:

- My choice of a java.util.BitSet as the board representation. This
  was actually a necessity - I tried other, more naive representations
  but ran out of time/space before reaching the answer above - so went
  with BitSet in the end.

- My custom rolling of a number of map/mapcat replacements and use of
  java.util.Collections in place of [] and #{}. Clojure map functions
  are idiomatic and useful but return lazy sequences. I wanted my app
  to run efficiently in parallel so needed to avoid the
  synchronisation around the Futures used in the standard functions. I
  could not resist the 4x speed gain of using java.util.Collections,
  albeit it in a totally clojure-idiomatic way.

- count-seqs, partition-*, slice and parallel-breadth-first-search -
  these additions are needed to partition the data and parallelise the
  tree search. Your job spec emphasised functional experience and I
  wanted to highlight the inherent parallelism in the problem space
  and my solution and hence the applicability of a functional
  solution.

I thought about removing code relating to the last two issues, but
figured doing this at the last minute was unwise and that I might
aswell share it with you - so I have left it in at risk of appearing
to write overly complex code.


RUNNING:

Please 

- tar zxvf chess.tar.gz
- cd chess

you should find a directory hierarch like this:

./README.txt                                 - this file
./test  
./test/chess
./test/chess/core_test.clj                   - test suite
./src
./src/chess
./src/chess/core.clj                         - code
./project.clj                                - lein configuration
./target
./target/chess-0.1.0-SNAPSHOT-standalone.jar - runnable

When printing boards, pieces are shown K/Q/B/N/R, covered locations as
'+' (useful for checking program is working correctly) and free
locations as '-'.

Please run the program thus:

--------------------------------------------------------------------------------

[jules@megalodon chess]$ java -jar target/chess-0.1.0-SNAPSHOT-standalone.jar
chess: find all non-attacking solutions for given pieces on a board of given dimensions

 Switches                       Default  Desc              
 --------                       -------  ----              
 -w, --width                    0        board width       
 -h, --height                   0        board height      
 -k, --kings                    0        number of kings   
 -q, --queens                   0        number of queens  
 -b, --bishops                  0        number of bishops 
 -r, --rooks                    0        number of rooks   
 -n, --knights                  0        number of knights 
 -p, --no-printing, --printing  true                       

problem: place () on a 0x0 board
solutions: 1 found in 14 millis

boards:


done

--------------------------------------------------------------------------------

[jules@megalodon chess]$ java -jar target/chess-0.1.0-SNAPSHOT-standalone.jar --width 3 --height 3 --kings 2 --rooks 1
chess: find all non-attacking solutions for given pieces on a board of given dimensions

 Switches                       Default  Desc              
 --------                       -------  ----              
 -w, --width                    0        board width       
 -h, --height                   0        board height      
 -k, --kings                    0        number of kings   
 -q, --queens                   0        number of queens  
 -b, --bishops                  0        number of bishops 
 -r, --rooks                    0        number of rooks   
 -n, --knights                  0        number of knights 
 -p, --no-printing, --printing  true                       

problem: place (K K R) on a 3x3 board
parallel processing: #<ArrayList [3, 3, 3, 3, 2, 2]> nodes
solutions: 4 found in 113 millis

boards:

+R+
+++
K+K

K++
++R
K++

K+K
+++
+R+

++K
R++
++K

done

--------------------------------------------------------------------------------

[jules@megalodon chess]$ java -jar target/chess-0.1.0-SNAPSHOT-standalone.jar --width 4 --height 4 --rooks 2 --knights 4
chess: find all non-attacking solutions for given pieces on a board of given dimensions

 Switches                       Default  Desc              
 --------                       -------  ----              
 -w, --width                    0        board width       
 -h, --height                   0        board height      
 -k, --kings                    0        number of kings   
 -q, --queens                   0        number of queens  
 -b, --bishops                  0        number of bishops 
 -r, --rooks                    0        number of rooks   
 -n, --knights                  0        number of knights 
 -p, --no-printing, --printing  true                       

problem: place (R R N N N N) on a 4x4 board
solutions: 8 found in 139 millis

boards:

+R++
N+N+
+++R
N+N+

+++R
N+N+
+R++
N+N+

R+++
+N+N
++R+
+N+N

++R+
+N+N
R+++
+N+N

N+N+
+R++
N+N+
+++R

N+N+
+++R
N+N+
+R++

+N+N
R+++
+N+N
++R+

+N+N
++R+
+N+N
R+++

done

--------------------------------------------------------------------------------

[jules@megalodon chess]$ java -Xmx4g -Xms4g -server -jar target/chess-0.1.0-SNAPSHOT-standalone.jar -w 6 -h 9 -k 2 -q 1 -b 1 -r 1 -n 1 --no-printing
chess: find all non-attacking solutions for given pieces on a board of given dimensions

 Switches                       Default  Desc              
 --------                       -------  ----              
 -w, --width                    0        board width       
 -h, --height                   0        board height      
 -k, --kings                    0        number of kings   
 -q, --queens                   0        number of queens  
 -b, --bishops                  0        number of bishops 
 -r, --rooks                    0        number of rooks   
 -n, --knights                  0        number of knights 
 -p, --no-printing, --printing  true                       

problem: place (K K Q B R N) on a 6x9 board
parallel processing: #<ArrayList [210, 210, 210, 210, 209, 209]> nodes
solutions: 20136752 found in 4933 millis

[jules@megalodon chess]$ 

--------------------------------------------------------------------------------
just for completeness - here is a log of the test suite running:
--------------------------------------------------------------------------------

lein test

lein test chess.core-test
problem: place (Q) on a 1x1 board
parallel processing: #<ArrayList [1, 0, 0, 0, 0, 0]> nodes
solutions: 1 found in 14 millis

problem: place (Q Q) on a 2x2 board
solutions: 0 found in 2 millis

problem: place (Q Q Q) on a 3x3 board
solutions: 0 found in 5 millis

problem: place (Q Q Q Q) on a 4x4 board
solutions: 2 found in 6 millis

problem: place (Q Q Q Q Q) on a 5x5 board
solutions: 10 found in 9 millis

problem: place (Q Q Q Q Q Q) on a 6x6 board
solutions: 4 found in 21 millis

problem: place (Q Q Q Q Q Q Q) on a 7x7 board
solutions: 40 found in 120 millis

problem: place (Q Q Q Q Q Q Q Q) on a 8x8 board
solutions: 92 found in 990 millis

problem: place (Q Q Q Q Q Q Q Q Q) on a 9x9 board
solutions: 352 found in 9982 millis

problem: place (B) on a 1x1 board
parallel processing: #<ArrayList [1, 0, 0, 0, 0, 0]> nodes
solutions: 1 found in 3 millis

problem: place (B B) on a 2x2 board
solutions: 4 found in 0 millis

problem: place (B B B) on a 3x3 board
solutions: 26 found in 0 millis

problem: place (B B B B) on a 4x4 board
solutions: 260 found in 3 millis

problem: place (B B B B B) on a 5x5 board
solutions: 3368 found in 22 millis

problem: place (B B B B B B) on a 6x6 board
solutions: 53744 found in 450 millis

problem: place (B B B B B B) on a 7x7 board
solutions: 692320 found in 4708 millis

problem: place (B B B B B B) on a 8x8 board
solutions: 5599888 found in 33901 millis

problem: place (N N) on a 2x2 board
solutions: 6 found in 1 millis

problem: place (N N) on a 3x3 board
solutions: 28 found in 1 millis

problem: place (N N) on a 4x4 board
solutions: 96 found in 1 millis

problem: place (N N) on a 5x5 board
solutions: 252 found in 3 millis

problem: place (N N N N N) on a 5x5 board
solutions: 9386 found in 37 millis

problem: place (N N N N N N N N N N N N N N N) on a 6x6 board
solutions: 2560 found in 23799 millis

problem: place (R) on a 1x1 board
parallel processing: #<ArrayList [1, 0, 0, 0, 0, 0]> nodes
solutions: 1 found in 2 millis

problem: place [K K R] on a 3x3 board
parallel processing: #<ArrayList [3, 3, 3, 3, 2, 2]> nodes
solutions: 4 found in 5 millis

problem: place [R R N N N N] on a 4x4 board
solutions: 8 found in 4 millis

problem: place [Q B R N K K] on a 5x5 board
parallel processing: #<ArrayList [38, 38, 38, 38, 38, 38]> nodes
solutions: 1088 found in 6 millis

problem: place [Q B R N K K] on a 6x6 board
parallel processing: #<ArrayList [87, 87, 87, 87, 86, 86]> nodes
solutions: 180568 found in 120 millis

problem: place [Q B R N K K] on a 9x6 board
parallel processing: #<ArrayList [210, 210, 210, 210, 209, 209]> nodes
solutions: 20136752 found in 6858 millis

problem: place [A B C] on a 1x2 board
solutions: 6 found in 0 millis


Ran 36 tests containing 111 assertions.
0 failures, 0 errors.

--------------------------------------------------------------------------------
