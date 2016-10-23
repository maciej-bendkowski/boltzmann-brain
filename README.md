# Boltzmann Brain

```Boltzmann Brain``` is a Haskell-based library and combinatorial system
compiler. Using a context-free text input representing a combinatorial system of
unlabelled structures, ```Boltzmann Brain``` makes it easy to auto-magically
construct a working, self-contained Haskell module implementing a rejection
Boltzmann sampler.

#### Disclaimer
As its currently under constant development, ```Boltzmann Brain``` lacks some
more advanced features. Currently implemented features include:
- unlabelled combinatorial systems of varying constructor weights,
- singularity approximation and system evaluation,
- working, self-contained Haskell module utilizing ```Control.Monad.Random```
  and ```Control.Monad.Trans.Maybe``` in the implementation of a dedicated
Boltzmann sampler,
- simple, context-free text-based input API.

#### TODO List
- fast Newton oracle,
- support for a list-based ```SEQ``` constructor,
- well-foundness check,
- documentation.

#### Installation and usage
```Boltzmann Brain``` is developed using ```stack```. For usage details please
see the ```example``` directory and/or type ```bb -h```.
