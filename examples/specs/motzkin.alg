-- Motzkin trees

@module    Sampler
@precision 1.0e-12
@maxiter   30

@withIO    y
@withLists y
@withShow  y

@colorScheme black
@size 100

M = Leaf | Unary M [30] | Binary M M.
