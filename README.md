Boltzmann Brain
---------------

*Boltzmann Brain* is a Haskell-based library and combinatorial system
compiler [1]. Using an easy and intuitive context-free text input representing 
a combinatorial system of rational or algebraic structures, *Boltzmann Brain* constructs 
a working, self-contained Haskell module implementing a dedicated
singular,  rejection-based Boltzmann sampler [2].

#### How to install

*Boltzmann brain* requires several pre-installed tools.
 * The tuning part requires `python2`. If you don't have it, visit [this page](https://wiki.python.org/moin/BeginnersGuide/Download).
 * Within `python2` several additional packages should be installed. Normally when you launch the code, it tells you the exact list if some package is missing. In order to install the packages, type into the command line
 ```
 pip2 install cvxpy numpy sympy matplotlib
 ```
 Note that the last three packages come by default with [Scientific Computing Tools for Python](https://www.scipy.org/about.html)
 * The compiling and sampling part require `Haskell`. We recommend to use [haskell-stack](https://docs.haskellstack.org/en/stable/README/) as state-of-the-art package manager for Haskell. It will automatically use the preferred version of `ghc` (Glasgow Haskell Compiler) and corresponding versions of required packages. Otherwise one can use `cabal` on the top of which `stack` is developed.
 * Once `stack` in installed, clone this repository with `git` (or download it manually)
 ```
 git clone https://github.com/maciej-bendkowski/boltzmann-brain.git
 ```
 and enter the folder containing `Setup.hs`.
 Type
 ```
 stack solver
 ```
 If some problems are encountered, add the flag `--update-config`, i.e. type
 ```
 stack solver --update-config
 ```
 This will configure the packages required for your particular system that are missing. Then consequently type
 ```
 stack build
 stack install
 ```
 This will install `boltzmann-brain` into your system. Type `bb -h` to check that it works.
 * In order to install the tools for tuning weights of combinatorial systems, enter the folder `paganini` and type
 ```
 pip2 install cvxpy numpy sympy matplotlib
 python2 setup.py install
 ```
 
##### Troubleshooting
On `Mac OS` older versions like `10.9` package managers like `brew` can only install `stack` from source.
This takes a long time, presubamly over one day.
In some cases it is faster to completely update the operational system before attempting to install some of the prerequisites.

The `hmatrix` package in `Haskell` requires prominent linear algebra packages `LAPACK` and `BLAS` (which are sometimes called "one of the achievements of the human species"). You can follow the instructions on the [official website](http://www.netlib.org/lapack/).

#### Usage

After installing `boltzmann-brain` and `paganini` it is possible to obtain help about each of the applications by typing
```
bb -h
paganini -h
```

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
By default, the package `paganini` which tunes multiparametric combinatorial system is not called.
This happens only when the user specifices the weights inside the square brackets.

To tune a combinatorial specification, we start with generating a *Paganini*
representation of the system, e.g. using 

```bb -g specification.in > paganini.pg```.

Boltzmann Brain ensures that the input specification is sound and well-founded.
Next, we run 

```paganini -i paganini.pg > bb.param```

which executes Paganini and generates required parameters for Boltzmann Brain.
You can alter the default agruments of `paganini` scripts like tuning precision,
optimisation problem solver, maximum number of iterations, and explicitly specify
if the type of the grammar is rational (because the problem becomes unbounded and the user may receive an error in this case).

 Finally, we need to tell Boltzmann Brain to use the parameters running, e.g.:

```bb --with-io -m Sampler -t bb.param specification.in > Sampler.hs```

#### References
1. [P. Flajolet, R. Sedgewick: Analytic Combinatorics](http://algo.inria.fr/flajolet/Publications/book.pdf)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for the random generation of combinatorial structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [C. Pivoteau, B. Salvy, M. Soria: Algorithms for Combinatorial Systems: Well-Founded Systems and Newton Iterations](https://arxiv.org/abs/1109.2688)
