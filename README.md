# pebbli_game
Pebbli Game in LISP - Artificial Intelligence

The objective of the work is to implement in LISP, using the
paradigm of functional programming, the resolution of
of the game Pebbli (http://mlagerberg.com/pebbli), using two
search algorithms.

Implementation:
1. Abstract Information Type (TAI) Pebbli. It allows at least create;
Consult; print out; copy; check if they are the same; movements and calculate
successors.
2. An uninformed search algorithm to solve Pebbli.
3. The algorithm A * to solve the Pebbli. Important:
· Creation of a heuristic function.
· Creation of efficient data structures to store the nodes of the search tree.
· Verification of repeated states (tree cycles).
