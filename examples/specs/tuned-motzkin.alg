-- Motzkin trees
@size 100000
MotzkinTree = Leaf
            | Unary MotzkinTree (2) [30000]
            | Binary MotzkinTree MotzkinTree (2).
