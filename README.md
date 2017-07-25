Boltzmann Brain
---------------

*Boltzmann Brain* is a Haskell-based library and combinatorial system
compiler [1]. Using an easy and intuitive context-free text input representing 
a combinatorial system of rational or algebraic structures, *Boltzmann Brain* constructs 
a working, self-contained Haskell module implementing a dedicated
singular,  rejection-based Boltzmann sampler [2].

#### Input
The input format mimics that of Haskell algebraic data types where in addition each
constructor may be annotated with an additional *weight* parameter. For instance:

```hs
-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2)
            | Binary MotzkinTree MotzkinTree.
   ```
In the example, a ```MotzkinTree``` type is defined. It contains three constructors: a constant ```Leaf``` of weight one (default value if not annotated), a unary ```Unary``` constructor of weight two and a binary contructor ```Binary``` of default weight one. The definition ends with an obligatory dot.

Each definition constitutes an algebraic data type where each inhabitant has an intrinsic *size*, defined as the sum of all its building constructor weights. 
*Boltzmann Brain* automatically detects the input system variant and generates an efficient singular, rejection-based Boltzmann samplers able to sample *uniformly random*,
 with respect to size, inhabitants of the system types. Though the exact size of the outcome is a random variable, the generated sampler allows to control the 
desired lower and upper bounds. 

See the *examples* directory for more examples of supported inputs and corresponding sampler modules.

#### Features
- easy and intuitive text-based input format;
- automated well-foudness check for the given combinatorial specification (see [3]);
- efficient system evaluation and singularity approximation using the numerical Newton oracle (see [3]);
- working, self-contained Haskell module generation utilising ```Control.Monad.Random```
  and ```Control.Monad.Trans.Maybe``` in the implementation of the constructed Boltzmann sampler;
- automated constructor frequency tuning and singularity approximation using *Paganini*;
- support for algebraic and strongly connected regular specifications;
- built-in support for the admissible list-based sequence constructor;
- syntactic sugar for lists and tuples (see below)

```hs
Tree = Node [Tree].
TreeTuple = (Tree, Tree, Tree).
TreeTupleList = [TreeTuple].
```

#### Frequency tuning
*Boltzmann Brain* supports a target frequency calibration using convex optimisation techniques included in the suplementary 
*Paganini* script (see the *paganini* subdirectory). Consider the following example of a specification defining
 Motzkin trees with some arbitrary size notion:

```hs
-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2) [0.3]
            | Binary MotzkinTree MotzkinTree (2).
   ```
Here, the ```Unary``` construct is given weight *2* and a target frequency
of *0.3*. In consequence, the system is to be *tuned* such that the ```Unary``` node 
contributes, on average, *30%* of the total size of constructed Motzkin trees.
It is hence possible to distort the natural frequency of each constructor 
in the given system. However, such an additional non-trivial tuning procedure causes a
not insignificant change in the underlying Boltzmann probaility model. In extremal cases, such as for instance
requirng *80%* of internal nodes in plane binary trees, the constructed sampler might 
be virtually ineffective due to the sparsity of tuned structures.

#### Using Boltzmann Brain with Paganini
To tune a combinatorial specification, we start with generating a *Paganini*
representation of the system, e.g. using 

```bb -g specification.in > paganini.pg```.

Boltzmann Brain ensures that the input specification is sound and well-founded.
Next, we run 

```paganini paganini.pg 1e-09 > bb.param```

which executes Paganini and generates required parameters for Boltzmann Brain.
We can control, for instance, the used optimisation method or the numerical precision.
 Finally, we need to tell Boltzmann Brain to use the parameters running, e.g.:

```bb --with-io -m Sampler -t bb.param specification.in > Sampler.hs```

#### Installation and detailed usage
*Boltzmann Brain* is developed using ```stack``` on top of ```cabal```.
For usage details please consult the *examples* directory and/or type ```bb -h```.
*Paganini* is developed using *cvxpy*, *sympy* and *numpy* on top of *python2*.
For usage and installation details please see the *paganini* directory.

#### References
1. [P. Flajolet, R. Sedgewick: Analytic Combinatorics](http://algo.inria.fr/flajolet/Publications/book.pdf)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for the random generation of combinatorial structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [C. Pivoteau, B. Salvy, M. Soria: Algorithms for Combinatorial Systems: Well-Founded Systems and Newton Iterations](https://arxiv.org/abs/1109.2688)
