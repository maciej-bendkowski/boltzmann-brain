# flakes does not need a shell.nix file (or default.nix)
# This file is for compatibility with tools that need a shell.nix file, e.g. `lorri`.
(import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz) {
  src = ./.;
}).shellNix.default
