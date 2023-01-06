{
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    with flake-utils.lib;
      eachDefaultSystem (system: let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;
        regen = pkgs.writeShellApplication {
          name = "regen";
          ## coreutils for xargs
          runtimeInputs = with pkgs; [cabal2nix findutils coreutils];
          text = ''
            set -o errexit
            set -o pipefail
            set -o xtrace
            ## Nix automatically runs `shellcheck` on this script
            # shellcheck disable=SC2016
            find . -name '*.cabal' -print0 | \
            xargs --null -I{} \
            sh -c 'cabal2nix "$(dirname {})" > "$(basename --suffix='.cabal' {}).nix"'
          '';
        };
        smtlib-backends = hpkgs.callPackage ./smtlib-backends.nix {};
        smtlib-backends-process = hpkgs.callPackage ./smtlib-backends-process.nix {
          inherit smtlib-backends smtlib-backends-tests;
        };
        smtlib-backends-tests = hpkgs.callPackage ./smtlib-backends-tests.nix {inherit smtlib-backends;};
        smtlib-backends-z3 = hpkgs.callPackage ./smtlib-backends-z3.nix {
          inherit smtlib-backends smtlib-backends-tests;
        };
      in {
        formatter = pkgs.alejandra;

        ## Generate derivations for Haskell packages.
        ## We use `cabal2nix` from shell rather than the Nix-level 
        ## `callCabal2nix` to avoid using `allow-import-from-derivation`
        ## (which allows this flake to e.g. support different operating
        ## systems)
        apps.regen = {
          type = "app";
          program = "${regen}/bin/regen";
        };

        devShells = let
          ## Needed by Z3 tests and haskell language server
          LD_LIBRARY_PATH = with pkgs; lib.strings.makeLibraryPath [z3];
          packages = p: [
            smtlib-backends
            smtlib-backends-tests
            smtlib-backends-process
            smtlib-backends-z3
          ];
        in {
          default = hpkgs.shellFor {
            inherit packages;

            withHoogle = true;

            buildInputs =
              (with hpkgs; [
                cabal-install
                hlint
                haskell-language-server
                cabal-fmt
              ])
              ++ [pkgs.z3];

            inherit LD_LIBRARY_PATH;
          };

          ## Lightweight development shell.
          lightweight = hpkgs.shellFor {
            buildInputs = with pkgs; [ghc cabal-install z3];
            inherit packages;
            inherit LD_LIBRARY_PATH;
          };
        };
      });
}
