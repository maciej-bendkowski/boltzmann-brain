-- A regular language which does have a strongly connected dependency graph.
A = Cons1 B | Cons2 A.
B = Cons3 A | Cons4.
