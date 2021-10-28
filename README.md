Boltzmann Brain [![Build
Status](https://travis-ci.org/maciej-bendkowski/boltzmann-brain.svg?branch=master)](https://travis-ci.org/maciej-bendkowski/boltzmann-brain)
[![Hackage](https://img.shields.io/badge/hackage-v1.6-blue.svg)](http://hackage.haskell.org/package/boltzmann-brain)
[![License](https://img.shields.io/badge/license-BSD--3-orange.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
---------------

*Boltzmann Brain* is a [Haskell](https://www.haskell.org/) library and set of
standalone applications meant for random generation of combinatorial structures.
Using an **easy** and **intuitive** context-free text input representing a
combinatorial specification of rational or algebraic objects, *Boltzmann Brain*
allows its users to:

- **sample** random structures following the given input specification, and
- **compile** a self-contained, dedicated analytic sampler for **optimal sampling efficiency**.

Remarkably, using *Boltzmann Brain* it is possible to **control the outcome
distribution** of generated objects and, in particular, skew it to one needs.
You provide the target distribution, we handle the sampling process for you!

**If you can specify it, you can sample it!**

![Random tree](https://github.com/maciej-bendkowski/boltzmann-brain/blob/master/examples/images/tree-large.png)

1. [Overview](#overview)
2. [Sampler tuning](#sampler-tuning)
3. [Basic usage](#basic-usage)
4. [Advanced usage](#advanced-usage)
5. [Installation](#installation)
6. [References](#references)

### Citing Boltzmann Brain
If you use *Boltzmann Brain* or its components for published work,  we encourage you to cite the accompanying paper:

*Maciej Bendkowski, Olivier Bodini, Sergey Dovgal*

[Polynomial tuning of multiparametric combinatorial
samplers](https://epubs.siam.org/doi/10.1137/1.9781611975062.9)

### Overview

*Boltzmann Brain* (*bb* for short) is an open-source analytic sampler compiler.
Given a textual representation of the combinatorial system, *bb* constructs a
dedicated **analytic sampler**. The sampler itself is a self-contained, and
reusable [Haskell](https://www.haskell.org/) module which, by construction, is
**guaranteed** to sample random objects following the given, feasible target
distribution. Using state-of-the-art optimization techniques, *bb* compiles an
**efficient** sampler implementation which can be further modified, incorporated
in other software or used as a standalone module.

The input specification format mimics that of [Haskell algebraic data
types](https://wiki.haskell.org/Algebraic_data_type) where in addition each type
constructor may be annotated with an additional *weight* parameter. For
instance:

```hs
-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2)
            | Binary MotzkinTree MotzkinTree.
   ```
In the above example, a ```MotzkinTree``` data type is defined. It contains three
constructors:

- a constant ```Leaf``` of weight one (default value if not annotated);
- a unary ```Unary``` constructor of weight two, and
- a binary constructor ```Binary``` of default weight one.

The definition ends with an obligatory dot.

Each definition constitutes an algebraic data type where each inhabitant has an
intrinsic *size*, defined as the sum of all its building constructor weights.

Given a system of (possibly) mutually recursive types, *Boltzmann Brain*
automatically detects the specification kind (either **rational** or
**algebraic**) and constructs a **rejection-based analytic sampler** able to
sample **uniformly random**, conditioned on size, inhabitants of the system
types. Though the exact size of the outcome is a random variable, the outcome
sampler allows its user to control the desired lower and upper bounds of the
generated objects.

### Sampler tuning
*Boltzmann Brain* supports **target frequency calibration** using convex
optimisation techniques. These are implemented as a Python library *Paganini*
built using *cvxpy*. *Boltzmann Brain* communicates with *Paganini* through a
custom middleware and EDSL called *Paganini-hs*.

Consider the following example of a specification defining Motzkin trees with
some arbitrary size notion:

```hs
@module   Sampler
@size     1000

@generate MotzkinTree 

-- Motzkin trees
MotzkinTree = Leaf
            | Unary MotzkinTree (2) [250]
            | Binary MotzkinTree MotzkinTree (2).
   ```
Using the ```@size``` and ```@generate``` annotation, we indicate the *target size* and *output type*.
The ```@module``` annotation defines the name of the outcome sampler module.
The ```Unary``` constructor is given weight *2* and a target frequency of
*250*. In consequence, the system is to be **tuned** such that the size
of generated Motzkin trees is on average *1000*. The
total weight of ```Unary``` nodes is, in expectation, *250*.

It is therefore possible to distort the natural frequency of each constructor in
the given system. However, such an additional non-trivial tuning procedure
causes a not insignificant change in the underlying probability model. In
extreme cases, such as for instance requiring *80%* of internal nodes in plane
binary trees, the sampler might be unavailable or virtually ineffective due to
the sparsity of tuned structures.

**Please tune with caution!**

### Basic usage
*Boltzmann Brain* ships with a few executables. To generate a sampler module you
can use ```bb-compile``` (see ```bb-compile -h``` for standard help/usage hints).
More advanced options, *Boltzmann Brain* provides its own annotation system (see example below):
```hs
-- Motzkin trees
@module    Sampler
@precision 1.0e-12
@maxiter   30

@withIO    y
@withLists y
@withShow  y

@generate M
@size 100

M = Leaf
  | Unary M [30]
  | Binary M M.
 ```
The `@module` annotation controls the name of the generated Haskell module (it
defaults to `Sampler` if not explicitly given).  Next two annotations
`@precision` and `@maxiter` are parameters passed to *Paganini* and control the
quality of the tuning procedure.  If not provided, some reasonable default values are
assumed (depending on the detected system type). The last three parameters
control some additional parameters used while generating the sampler code.
Specifically, whether to generate additional `IO` (input/output) generators, whether to generate
list samplers for each type in the system, and finally whether to include
`deriving Show` clauses for each type in the system. By default, `@withIO` and
`@withShow` are enabled (to disable them, set them to `n` or `no`); `@withLists`
is by default disabled if not stated otherwise in the input specification.

For parameter tuning, without sampler construction, you can use ```bb-tune```.
To tune a combinatorial specification *"by hand"*, you can invoke

```bb-tune -i specification.in -o specification.json```.

*Boltzmann Brain* ensures that the input specification is sound and well-founded.
Otherwise, some custom user-friendly error messages are provided. The *tuned* system
is converted into a suitable JSON representation and is ready for further manipulation,
for instance sampler generation.

### Sampler compilation

In most cases, it is preferable to generate sampler **executables* instead of Haskell modules.
For such cases you can use the ```bb-build``` python script in ```scripts/bin/```. We recommend
you include this path in your ```PATH```. See ```bb-help --help``` for more usage details.

### Installation
*Boltzmann Brain* consists of serveral executables. They are implemented in [Haskell](https://www.haskell.org/) whereas the **Paganini**, on which they depend on, is implemented in [Python](https://www.python.org/). Both of these parts rely on (few) external libraries to work, such as [LAPACK](http://www.netlib.org/lapack/) or [BLAS](http://www.netlib.org/blas/). The following sections explain several common installation methods.

We start with the recommended method of compiling *Boltzmann Brain* from sources.
The following section explains the compilation process under Ubuntu 16.04, however, let us note that with little modifications it should also work under other Linux distributions.

#### Linux (Ubuntu 16.04 or newer)
We start with installing all the required system dependencies:
```
apt-get update
apt-get install -y cmake curl git libblas-dev liblapack-dev python3 python3-pip
```
The above script installs ```BLAS``` and ```LAPACK``` as well as python3 and its package manager ```pip```.  Since the target destination of executables is going to be ```~/.local/bin```, before proceeding please make sure that it is included in your ```PATH```.

Next, we install required python dependencies

```
 pip3 install --user --upgrade pip
 pip3 install --user numpy scipy
 pip3 install --user cvxpy
 pip3 install --user paganini
```

Note that the above packages play the central role in the system tuning procedure. Without them, *paganini* cannot not work properly. Next, we clone the current repository

```
git clone https://github.com/maciej-bendkowski/boltzmann-brain.git
```

Now, we have to prepare to install *Boltzmann Brain*. For that purpose, we are going
to download Haskell's [Stack](https://docs.haskellstack.org/en/stable/README/) tool chain. Note that stack is able to download and isolate various [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) instances, avoiding the infamous [Cabal hell](https://wiki.haskell.org/Cabal/Survival).

To install stack for linux x86_64 type

```
curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
```

Now, all we need is to go back to the main boltzmann-brain folder and use stack install:
```
stack setup --resolver=lts-14.16
stack install happy --resolver=lts-14.16
stack install
```

#### Nix support (linux only)

[nix](https://nixos.org/) is a package manager that works under linux and macos. Support for installing dependencies and a assembling a development environment for building and running boltzmann-brain is provided by the `shell.nix` file in the top-level directory. To use it, you need to have nixpkgs installed already. Then you can install boltzmann-brain as follows:

````
git clone https://github.com/maciej-bendkowski/boltzmann-brain.git
nix-shell
cabal build 
````

The second command enters shell which provides python, paganini, and all the tooling needed to build boltzmann-brain using ghc and cabal. The last command builds the library and executatables. 

Additional Information:
* Nix support was tested under nixos with ghc 8.6.5. Other version of ghc *will not* work. 
* Nix support is provided via nix flakes, an experimental feature. The settings for the development environment are contained in flakes.nix, with additional version pinning information contained in flake.lock. Users with nix flakes enabled can enter `nix develop` and ignore the shell.nix file.
* See [these introductory blog posts](https://www.tweag.io/blog/2020-05-25-flakes/) for more information about nix flakes.


#### macOS >= 10.12

In the following section we explain how to compile *Boltzmann Brain* in OSX.

* First, you need `python` to be installed (both versions 2 or 3 should be fine).
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
   pip install --upgrade six numpy sympy cvxpy
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

 Note that the last two packages come by default with [Scientific Computing
Tools for Python](https://www.scipy.org/about.html)

#### Pre-compiled binaries

We use Travis CI in order to build ```bb``` for Linux and OSX, both in the
x86_64 architecture. Pre-compiled binaries of ```bb``` are available at out  [releases webpage](https://github.com/maciej-bendkowski/boltzmann-brain/releases).

#### Package managers

We offer [Hackage](https://hackage.haskell.org/package/boltzmann-brain-1.4). You can install it by typing
```
stack install boltzmann-brain
```

### References
*Boltzmann Brain* heavily relies on published work of numerous excellent authors. Below, you can find a short (and definitely inexhaustive) list of papers on the subject:

- [P. Flajolet, R. Sedgewick: Analytic
   Combinatorics](http://algo.inria.fr/flajolet/Publications/book.pdf)
- [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for
   the random generation of combinatorial
structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
- [C. Pivoteau, B. Salvy, M. Soria: Algorithms for Combinatorial Systems:
   Well-Founded Systems and Newton Iterations](https://www.sciencedirect.com/science/article/pii/S0097316512000908)
- [O.Bodini, J. Lumbroso, N. Rolin: Analytic samplers and the combinatorial rejection method](https://dl.acm.org/citation.cfm?id=2790220&dl=ACM&coll=DL)
- [F. Saad, C. Freer, M. Rinard, V. Mansinghka : Optimal Approximate Sampling from Discrete Probability Distributions](https://arxiv.org/pdf/2001.04555.pdf)
