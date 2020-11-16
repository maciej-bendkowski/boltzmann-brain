-- Boltzmann sampler for Boolean functions.

@precision 1.0e-12
@maxiter   30

@withIO    y
@withLists y
@withShow  y

@size 100000
@generate Fun

-- variables.
Index = Succ Index
      | Zero.

-- Boolean functions.
Fun = Variable Index (0)
    | And Fun Fun [30000]
    | Or Fun Fun
    | Neg Fun.
