{
  description = "nix support for boltzmann-brain";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixos-20.03"; 

  inputs.nixpkgs-unstable.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mach-nix.url = "github:DavHau/mach-nix";
  
  inputs.paganini-hs.url = "github:maciej-bendkowski/paganini-hs/c95369b1fc245ae943a0dbc90647688128ab5c71";
  inputs.paganini-hs.flake = false;

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, flake-utils, mach-nix, paganini-hs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "boltzmann-brain";

        # paganini-hs dependency BinderAnn breaks if ghc > 8.65 
        compiler = "ghc865";  
         
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
        };

        unstable = import nixpkgs-unstable {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
        };

        mylib = mach-nix.lib.${system}; # adds mkPython, mkPythonShell, etc.

        # latest scs breaks on `import paganini`, 
        paganini-custom = mylib.mkPython {
          requirements = ''
            paganini==1.3.3
            cvxpy>=1.1.7 
            scs==2.1.1-2 # scs 2.1.2 broken 
          '';
        };

        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            
            boltzmann-brain = self.callCabal2nix "boltzmann-brain" ./. { };

            # 1.22 is a breaking change. Must stop at 1.21.1.
            haskell-src-exts = super.callHackage "haskell-src-exts" "1.21.1" {};

            # from pinned github, nixos-20.03 didn't have it yet. 
            BinderAnn = super.callHackageDirect
              { pkg = "BinderAnn";
                ver = "0.1.0.0";
                sha256 = "f3EOxiOS1OJTohanKzfpeLfacgUM0VlEqOj5e+kHAuI=";} {};

            paganini-hs = pkgs.haskell.lib.dontCheck (
              super.callCabal2nix "paganini-hs" paganini-hs {});
          };
        };

        devShell = haskellPackages.shellFor {
          withHoogle = false; # Provides docs, optional. 
          packages = p: [
            p.boltzmann-brain
          ]; 
          buildInputs = [
            unstable.cabal-install
            unstable.ghcid
            unstable.haskell-language-server
            unstable.hlint
            unstable.ormolu
            unstable.cabal2nix
            paganini-custom
          ];
        };
        
        drv = haskellPackages.boltzmann-brain;
        
      in
        rec {
          inherit devShell;
          defaultPackage = drv;
        });
}
