## Paganini: Convex optimisation tuner for combinatorial systems

```
Install: python3 setup.py install
Usage: python3 paganini.py -i input.txt -p 1e-6
Usage: python3 paganini.py -i input.txt -s CVXOPT
Usage: python3 paganini.py -i input.txt -s SCS
Usage: python3 paganini.py -i input.txt -s ECOS
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

We want to have 40% of abstractions, so we encode all the variables and
functions into a single vector [z, u, L, D] and start with
the virtual specification:

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
```

Here, the first line specifies the total number of equations and the number
of marking variables, respectively. The following line lists the target
frequencies of corresponding marking variables. Next follows the description
of the system equations.  For each equation we write down the number k of
right-hand side summands. After that, we write k lines denoting the
exponents of corresponding variables. And so, for instance, the first line
in the first equation in the above system specification encodes the
expression

```
z^1 * u^1 * L^1 * D^0
```

Such a virtual system description is finally 'sparsified' as follows:

```
2 1
0.4
3
(1,0) (1,1) (1,2)
(1,0) (2,2)
(1,3)
2
(1,0)
(1,0) (1,3)
```

In other words, for each equation we write down only its non-zero monomials
with respective exponents. Such an input format encodes the system
corresponding to lambda-terms.
