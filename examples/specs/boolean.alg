-- Boltzmann sampler for Boolean functions.

@module    Boolean
@precision 1.0e-12
@maxiter   30

@withIO    y
@withLists y
@withShow  y

-- variables.
Index = Succ Index
      | Zero.

-- Boolean functions.
Fun = Variable Index (0)
    | And Fun Fun [0.3]
    | Or Fun Fun
    | Neg Fun.
