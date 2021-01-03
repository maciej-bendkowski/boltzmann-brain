{
  description = "nix support for boltzmann-brain";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable"; 
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mach-nix.url = "github:DavHau/mach-nix";
  
  inputs.paganini-hs.url = "github:maciej-bendkowski/paganini-hs";

  outputs = { self, nixpkgs, flake-utils, mach-nix, paganini-hs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "boltzmann-brain";

        # paganini-hs dependency BinderAnn breaks ghc>8.65 
        compiler = "ghc865";  
         
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          # overlays = [] # Add or tweak non-Haskell packages here.
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

        # 1.22 is a breaking change for us
        haskell-src-exts-custom = pkgs.haskell.lib.overrideCabal pkgs.haskell.packages.${compiler}.haskell-src-exts {
          version = "1.21.1";
          sha256 = "LskRYUMl8eXu9+W+8VwIuCZZMdado85WavEJ1IZFPmA=";
        };
        
        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            
            "${name}" = super.callCabal2nix name ./. {
              haskell-src-exts = haskell-src-exts-custom;
            };

            paganini-hs = super.callPackage paganini-hs.derive.${system} {
              paganini = paganini-custom;
            };

          };
        };

        devShell = haskellPackages.shellFor {
          withHoogle = false; # Provides docs, optional. 
          packages = p: [
            p."${name}"
          ]; 
          buildInputs = [
            haskellPackages.cabal-install
            pkgs.stack
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.cabal2nix
            paganini-custom
          ];
        };

        drv = haskellPackages."${name}";
        
      in
        rec {
          inherit devShell;
          defaultPackage = drv;
        });
}
