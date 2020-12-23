{
  description = "nix support for boltzmann-brain";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable"; 
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mach-nix.url = "github:DavHau/mach-nix";
  
  inputs.paganini-hs.url = "github:timsears/paganini-hs/nix-support";

  outputs = { self, nixpkgs, flake-utils, mach-nix, paganini-hs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "boltzmann-brain";

        # paganini-hs dependency BinderAnn won't compile with later ghc versions 
        compiler = "ghc882";  
         
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          # overlays = [] # Add or tweak non-Haskell packages here.
        };

        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            "${name}" = self.callCabal2nix name ./. {};
            # Override other Haskell packages as needed here.
          };
        };
        
        devEnv = haskellPackages.shellFor {
          withHoogle = true; # Provides docs, optional. 
          packages = p: [
            p."${name}"
            # Add other Haskell packages below if you just want a Haskell hacking env.
            # p.lens
          ]; 
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            cabal2nix
            # Add more dev tools as needed. They won't be included by `nix build`
          ];
        };

        drv = haskellPackages."${name}";
        
      in
        rec {
          inherit devShell;
          # defaultPackage = drv;
        });
}
