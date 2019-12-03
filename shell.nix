{ pin         ? true
, compiler    ? "default"
, doBenchmark ? false
, withHoogle  ? true }:

with builtins;
let
  pinFile = ./nixpkgs-pinned.nix; #contains commit hash. note: assumes it's a string in quotes
  pinCommit = import pinFile;
  pinOK = pin && (builtins.pathExists pinFile);

  #TODO use a local git repo instead to speed things up.
  pinnedPkgs = fetchGit {
                 name = "nixpkgs-pinned-" + (builtins.substring 0 10 pinCommit);
                 url = https://github.com/nixos/nixpkgs/;
                 rev = pinCommit;
                 };              

  pkgs = import (if pinOK 
                    then pinnedPkgs
                    else <nixpkgs>
                    ) {
                        config.allowUnfree = true;
                        config.allowBroken = true;
                        };

  pinMessage = if pinOK
                 then "pinned git hash:   ${pinnedPkgs.rev}"
                 else "To pin, add commit string to \n ${builtins.toString pinFile}";
                  
in 

with pkgs.haskell.lib;
let
  #inherit builtins;
  #inherit (pkgs.haskell.lib) overrideSrc;
 
  overrideSourceFromLocalGit = drv : repo: overrideSrc drv { src = fetchGit { url = repo; }; };
  
  hp = if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

  haskellPackages = hp.override {
    overrides = final: initial: {
       # examples...
       # FontyFruity = final.callPackage ../FontyFruity/default.nix { };
       # codex = pkgs.haskell.lib.doJailbreak initial.codex;
       # use fetchGit for much cleaner source directory.
       # call-haskell-from-anything = overrideSrc initial.call-haskell-from-anything
       #                            { src = fetchGit { url = ../call-haskell-from-anything; }; };
       };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  ghcWith = if withHoogle then haskellPackages.ghcWithHoogle else haskellPackages.ghcWithPackages;

  # no need to run cabal2nix by hand. But we need to enter the package name...
  
  drv = variant (haskellPackages.callCabal2nix "drv" (fetchGit { url = ./.;}) {} ); 

in

{ drv = drv;

  der = (pkgs.stdenv.mkDerivation rec {
    name  = "dev-env";
    buildInputs = [
      (with haskellPackages; (ghcWith (p: with p;
        drv.propagatedBuildInputs ++ [
          cabal-install
          hasktags
          haskdogs
          ghcid
        ])))] 
    ++ (with pkgs; [
      feh
      # System requirements.
      readline # needed on linux? doesn't hurt
      lsb-release # currently in nixpkgs-unstable but not in nixos-19.09
      # Python requirements (enough to get a virtualenv going).
      python3Full
      python3Packages.virtualenv
      python3Packages.pip
      python3Packages.setuptools
    ])
    ++ (with pkgs.python3Packages; [
      numpy
      scipy
      cvxpy
      mpmath
      sympy
    ]);
          
    env = pkgs.buildEnv {
      name = "shell-env";
      paths = buildInputs;
    };

    shellHook = ''
    export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
    #echo "HIE_HOOGLE_DATABASE=" $HIE_HOOGLE_DATABASE
    echo "${pinMessage}"
    echo "unpinned git hash:" $(git -C ~/nixpkgs rev-parse HEAD)
    #TODO make something like the following work...
    #echo "unpinned git hash:" $(git -C <nixpkgs> rev-parse HEAD)
    echo "nix-shell with pinned nixpkgs? ${pkgs.lib.boolToString(pinOK)}"

    # python stuff
    # Allow the use of wheels.
    SOURCE_DATE_EPOCH=$(date +%s)
    # Augment the dynamic linker path
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.readline}/lib  
    export PYTHONPATH=$PYTHONPATH:.
    virtualenv . 
    source ./bin/activate
    pip install --user paganini
    easy_install medulla
    '';
}
); }.der

