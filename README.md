Boltzmann Brain
---------------

*Boltzmann Brain* is a Haskell-based library and combinatorial system
compiler [1].  Using an easy and intuitive context-free text input representing 
a combinatorial system of unlabelled structures, *Boltzmann Brain* constructs 
a working, self-contained Haskell module implementing a dedicated rejection
Boltzmann sampler [2].

#### Input
The input format mimics that of Haskell algebraic data types where in addition each
constructor may be annotated with an additional *weight* parameter. For instance:

```hs
/* Motzkin trees */
MotzkinTree = Leaf
            | Unary MotzkinTree (2)
            | Binary MotzkinTree MotzkinTree.
   ```
In the example, a ```MotzkinTree``` type is defined. It contains three constructors: a constant ```Leaf``` of weight one (default value if not annotated), a unary ```Unary``` constructor of weight two and a binary contructor ```Binary``` of default weight one. The definition ends with an obligatory dot.

Each definition constitutes an algebraic data type where each inhabitant has an intrinsic *size*, defined as the sum of all its building constructor weights. *Boltzmann Brain* generates a rejection Boltzmann sampler able to sample *uniformly random*, with respect to size, inhabitants of the system types. Though the exact size of the outcome is a random variable, the generated sampler allows to control the desired lower and upper bounds. See the *examples* directory for more examples of supported inputs and corresponding sampler modules.

#### Features
- easy and intuitive text-based API,
- automated well-foudness check for the given combinatorial specification (see [3]),
- optimised Newton oracle (see [3]) for numerical system evaluation,
- singularity approximation within a desired error bound,
- working, self-contained Haskell module generation utilizing ```Control.Monad.Random``` and ```Control.Monad.Trans.Maybe``` in the implementation of dedicated Boltzmann sampler,
- text-based compilator annotating constructors with appropriate *branching probabilities*.

#### Installation and usage
*Boltzmann Brain* is developed using ```stack```. For usage details please
see the *examples* directory and/or type ```bb -h```.

#### TODO
- [ ] Support for a list-based Seq constructor,
- [ ] Provide a hackage package with haddock documentation,
- [ ] Support pointing operations and labelled structures.

#### References
1. [P. Flajolet, R. Sedgewick: Analytic Combinatorics](http://algo.inria.fr/flajolet/Publications/book.pdf)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for the random generation of combinatorial structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [C. Pivoteau, B. Salvy, M. Soria: Algorithms for Combinatorial Systems: Well-Founded Systems and Newton Iterations](https://arxiv.org/abs/1109.2688)
