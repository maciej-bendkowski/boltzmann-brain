Boltzmann Brain
---------------

*Boltzmann Brain* is a Haskell-based library and standalone combinatorial system
sampler compiler and generator [1]. Using an easy and intuitive context-free
text input representing a combinatorial system of rational or algebraic
structures, *Boltzmann Brain* constructs a working, self-contained Haskell
module implementing a dedicated singular,  rejection-based Boltzmann sampler
[2]. Using *Boltzmann Brain* is also able to generate random objects from the
given specification or even compute the Boltzmann branching probabilities for
further usage.

### How to install

#### macOS >= 10.12

* First, you need `python` to be installed (both versions 2 or 3 are fine).
If you don't have `python` installed, you can find it at
[https://wiki.python.org/moin/BeginnersGuide/Download](https://wiki.python.org/moin/BeginnersGuide/Download).

* Using the dedicated `python` package manager `pip` you can install the paganini framework
```
  pip install paganini
```
Normally this works, but if some packages are outdated, consider upgrading them.

* The *Boltzmann Brain* package `bb` can be installed with package manager `homebrew`
```
  brew tap electric-tric/bb
  brew install boltzmann-brain
```
If you don't have `homebrew`, you can download the binary file
from our [releases webpage](https://github.com/maciej-bendkowski/boltzmann-brain/releases).

#### Debian/Ubuntu

* If you don't have `python` consider installing it either from the
[official website](https://wiki.python.org/moin/BeginnersGuide/Download)
or using the package manager
```
  sudo apt-get install python
```
Note that `python3` is preferable over `python2` but our tool works with both of
them.

* Install the `python` package `paganini` using
```
  pip install paganini
``` 

* If you choose to download the `bb` binaries, then visit 
(https://github.com/maciej-bendkowski/boltzmann-brain/releases)[https://github.com/maciej-bendkowski/boltzmann-brain/releases].

* Alternatively, you can build `bb` from source, which requires `ghc` (Glasgow Haskell Compiler)
and `stack`, which is a state-of-the-art package manager for Haskell and can be
installed with
```
  sudo apt-get install stack
```
Then, Boltzmann Brain is installed by executing
```
  stack install boltzmann-brain
```

#### Building from source

If your operation system is not supported, consider building *Boltzmann Brain*
from source.
                                        
* You need `python` and `ghc` in order to build *Boltzmann Brain*. We
  recommend to use [haskell-stack](https://docs.haskellstack.org/en/stable/README/)
  as state-of-the-art package manager for Haskell.

* Once `stack` in installed, clone this repository with `git` (or download it
manually)
```
    git clone https://github.com/maciej-bendkowski/boltzmann-brain.git
```

* Then, build and install the package using `stack`:
```
  stack solver
  stack solver --update-config
  stack build
  stack install
```
This will install `boltzmann-brain` into your system. Type `bb -h` to check
that it works.

* Next, enter the `paganini` folder, and install the `paganini` script
```
  cd paganini
  python setup.py install
```

#### Troubleshooting
On `Mac OS` older versions like `10.9` package managers like `brew` can only
install `stack` from source because the binaries are not maintained.
This takes a long time. In some cases it is faster
to completely update the operating system before attempting to install some of
the prerequisites.

* For some versions of `python`, the package manager `pip` doesn't come by
  default. `pip` is already installed if you use `python2 >= 2.7.9`
or `python3 >= 3.4`. Otherwise see
[instructions](https://pip.pypa.io/en/stable/installing/).

* If some of your packages, for example `numpy` are installed but outdated, the
  installation process sometimes gives an error. For such packages try
```
   pip install --upgrade numpy sympy cvxpy
```

* The package manager `pip` should not be used with `sudo`. If you don't have
  the right to write to some specific directory, try adding `--user` flag, for
example
```
    pip install --user paganini
```

* Within `python`, several additional packages should be installed. Normally
   pip handles the dependencies, so you just need to execute
```
  pip install paganini
```
When you launch `paganini`, the program tells you the list of packages that are
missing. In order to install the packages, type into the command line
 ```
 pip install six cvxpy numpy sympy
 ```
 Note that the last two packages come by default with [Scientific Computing
Tools for Python](https://www.scipy.org/about.html)

* The `hmatrix` package in `Haskell` requires prominent linear algebra packages
`LAPACK` and `BLAS` (which are sometimes called "one of the achievements of the
human species"). For some systems these packages are already included, but if
they are not, you can follow the instructions on the [official
website](http://www.netlib.org/lapack/).

### Usage

After installing `boltzmann-brain` and `paganini` it is possible to obtain help
about each of the applications by typing
```
bb -h
paganini -h
```

### Input
The input format mimics that of Haskell algebraic data types where in addition each
constructor may be annotated with an additional *weight* parameter. For instance:

```hs
-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2)
            | Binary MotzkinTree MotzkinTree.
   ```
In the example, a ```MotzkinTree``` type is defined. It contains three
constructors: a constant ```Leaf``` of weight one (default value if not
annotated), a unary ```Unary``` constructor of weight two and a binary
contructor ```Binary``` of default weight one. The definition ends with an
obligatory dot.

Each definition constitutes an algebraic data type where each inhabitant has an
intrinsic *size*, defined as the sum of all its building constructor weights.
*Boltzmann Brain* automatically detects the input system variant and generates
an efficient singular, rejection-based Boltzmann samplers able to sample
*uniformly random*, with respect to size, inhabitants of the system types.
Though the exact size of the outcome is a random variable, the generated sampler
allows to control the desired lower and upper bounds. 

See the *examples* directory for more examples of supported inputs and
corresponding sampler modules.

### Features
- easy and intuitive text-based input format;
- automated well-foudness check for the given combinatorial specification (see [3]);
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

### Frequency tuning
*Boltzmann Brain* supports a target frequency calibration using convex
optimisation techniques included in the supplementary *Paganini* script (see the
*paganini* subdirectory). Consider the following example of a specification
defining Motzkin trees with some arbitrary size notion:

```hs
-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2) [0.3]
            | Binary MotzkinTree MotzkinTree (2).
   ```
Here, the ```Unary``` construct is given weight *2* and a target frequency of
*0.3*. In consequence, the system is to be *tuned* such that the ```Unary```
node contributes, on average, *30%* of the total size of constructed Motzkin
trees.  It is hence possible to distort the natural frequency of each
constructor in the given system. However, such an additional non-trivial tuning
procedure causes a not insignificant change in the underlying Boltzmann
probaility model. In extremal cases, such as for instance requirng *80%* of
internal nodes in plane binary trees, the constructed sampler might be virtually
ineffective due to the sparsity of tuned structures.

### Annotations
Since v1.3, *Boltzmann Brain* provides its own annotation system (see example below):
```hs
-- Motzkin trees

@module    Sampler
@precision 1.0e-12
@maxiter   30

@withIO    y
@withLists y
@withShow  y

M = Leaf | Unary M [0.3] | Binary M M.
 ```
The `@module` annotation controls the name of the generated Haskell module (it
defaults to `Sampler` if not explicitly given).  Next two annotations
`@precision` and `@maxiter` are parameters passed to *Paganini* and control the
quality of the tuning procedure.  If not provided, some default values are
assumed (depending on the detected system type). The last three parameters
control some additional parameters used while generating the sampler code.
Specifically, whether to generate addtional `IO` generators, whether to generate
list samplers for each type in the system, and finally whether to include
`deriving Show` clauses for each type in the system. By default, `@withIO` and
`@withShow` are enabled (to disable them, set them to `n` or `no`); `@withLists`
is by default disabled if not stated otherwise in the input specification.

Using annotations it is also possible to control the sampling parameters of
`bb`. Consider the following example:
```hs
-- Random sampling of Motzkin trees

-- Parameters for "tuning"
@precision 1.0e-12
@maxiter   30

-- Sampling parameters
@lowerBound 100
@upperBound 10000
@generate   M

M = Leaf
  | Unary M [0.3]
  | Binary M M.
 ```
In the above example, three more annotations are used. The first two dictate the
admissible size window of the generated object whereas the third one specifies
the type from which we want to generate. If no bounds are provided, `bb` uses
some (small) default ones. If no `@generate` annotation is provided, `bb`
assumes some default type.

### Using Boltzmann Brain with Paganini
Since v1.3, *Boltzmann Brain* automatically calls *Paganini* in order to tune
the sampler corresponding to the given input system. If no special handling is
required, it suffices therefore to have `paganini` available in the system; `bb`
will automatically pass it necessary data and retrieve the tuning vector.

```bb -o Test.hs examples/motzkin.in```

A manual tuning workflow is also supported. To tune a combinatorial specification "by hand",
we start with generating a *Paganini* representation of the system, e.g. using 

```bb -o paganini.pg -s specification.in```.

*Boltzmann Brain* ensures that the input specification is sound and well-founded.
Next, we run 

```paganini -i paganini.pg > bb.param```

which executes *Paganini* and generates a required tuning vector for `bb`.  You
can alter the default agruments of `paganini` scripts like tuning precision,
optimisation problem solver, maximum number of iterations, and explicitly
specify if the type of the grammar is rational (since the optimisation problem
becomes unbounded and the user may receive an error in this case).

 Finally, we need to tell `bb` to use the parameters running, e.g.:

```bb -o Sampler.hs -p bb.param examples/motzkin.in ```

### Citing Boltzmann Brain
If you use `Boltzmann Brain` or `Paganini` for published work, 
we encourage you to cite the accompanying paper: 

Maciej Bendkowski, Olivier Bodini, Sergey Dovgal

[Polynomial tuning of multiparametric combinatorial
samplers](https://epubs.siam.org/doi/10.1137/1.9781611975062.9)

available also on [arxiv](https://arxiv.org/abs/1708.01212).

### References
1. [P. Flajolet, R. Sedgewick: Analytic
   Combinatorics](http://algo.inria.fr/flajolet/Publications/book.pdf)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for
   the random generation of combinatorial
structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [C. Pivoteau, B. Salvy, M. Soria: Algorithms for Combinatorial Systems:
   Well-Founded Systems and Newton Iterations](https://arxiv.org/abs/1109.2688)
