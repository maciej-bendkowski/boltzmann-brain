# Changelog
Notable major changes since version *1.2* are listed below.

### [1.6.0]

Updated code to be compatible with 8.6.5.
Removed hard dependence on haskell-src-exts=1.17.1
Bumped other package minimums.
Added nix support. 

### [1.5.0] - 21-02-2019
- It is now possible to sample multiple structures using the built-in sampler.
  The number of structures is provided through a `samples` annotation.
- *Boltzmann brain* accepts two input specification formats - an algebraic
  format and a new rational input format. The latter input format is designed
  for rational specifications and yields a specific *transfer matrix* sampler.
- Generated samplers use the *Buffon machines* package. In particular,
  they base their non-deterministic decisions on precompiled Knuth-Yao decision
  trees. As a result, a lot less random bits are used and the sampelrs run faster.
- *Paganini* uses a new, sparse input format. Moreover, the solver uses
  sparse matrices and hence less memory.

### [1.4.0] - 27-07-2018
- The command line interface of ```bb``` has changed quite a bit. *Boltzmann Brain* can
  now be used as a standalone sampler, compiler, or even generate GraphViz dotfiles. Moreover,
  decorated specifications are also available in a clear JSON format.
- *Boltzmann Brain* provides now better error handling and messaging. In particular,
  each error is accompanied with an additional hint message, helping the users in fixing
  the problem.
- *Paganini* can now read from standard input if given the flag ```--stdin```.
- *Boltzmann Brain* has its own Travis CI pipline.

### [1.3.0] - 08-10-2017
- *Paganini* is now fully integrated with *Boltzmann brain*. By default, *bb*
  handles the data flow between both *paganini* and itself, providing a seamless
  usage experience. Manual workflow is still available via flags `-p` and `-s`.
- *Paganini* reads from stdin if no input file is given.
- *Boltzmann brain* automatically detects the system type and chooses a suitable
  *Paganini* tuning method.
- Input flags for *Boltzmann brain* were reorganised. An annotation mechanism
  is now supported in system specifications. Available parameters include the module
  name, tuning precision and various output sampler details. In particular, some
  common annotations such as `@withIO` and `@withShow` are enabled by default.
- A new `-o` flag redirecting *bb*'s output to a given output file is now available.
