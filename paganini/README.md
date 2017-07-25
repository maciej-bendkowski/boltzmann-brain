## Paganini: Convex optimisation tuner for combinatorial systems

```
Install: python2 setup.py install
Usage: python2 paganini.py input.txt 1e-6
Usage: python2 paganini.py input.txt CVXOPT
Usage: python2 paganini.py input.txt SCS
Usage: python2 paganini.py input.txt ECOS
```

  * `input.txt` is the name of the input file
with coefficients of algebraic
specifications
  * `1e-6` is a float number corresponding to precision
  * `[CVXOPT, SCS, ECOS]` stand for different convex optimization solvers.
ECOS is more preferrable for algebraic systems, SCS for rational.

## Example

Consider a system for marking abstractions in lambda-terms:

```
L = z L^2 + u z L + D
D = z + z D
```

We want to have `40%` of abstractions, so we encode all the variables and
functions into a single vector `[z, u, L, D]` and construct input file
```
2 1
0.4
3
1 1 1 0
1 0 2 0
0 0 0 1
2
1 0 0 0
1 0 0 1
