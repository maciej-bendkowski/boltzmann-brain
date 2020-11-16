-- Input specification for a "tuned" Motzkin tree sampler.

@precision 1.0e-12
@maxiter   30

@generate     Motzkin
@size         1000

Motzkin = Leaf
        | Unary Motzkin [400]
        | Binary Motzkin Motzkin.
