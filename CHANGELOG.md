# Changelog
Notable major changes since version *1.2* are listed below.

## [1.3.0] - 08-10-2017
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
