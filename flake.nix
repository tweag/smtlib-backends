{
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskellPackages;
      makeBackendsDerivations = pkgs.writeShellApplication {
        name = "makeBackendsDerivations";
        ## coreutils for xargs
        runtimeInputs = with pkgs; [cabal2nix findutils coreutils];
        text = ''
          set -o errexit
          set -o pipefail
          set -o xtrace
          function runCabal2nix () {
            path="$(dirname "$1")"; file="$(basename --suffix='.cabal' "$1")"
            { echo '## This file has been generated automatically.'
              # shellcheck disable=SC2016
              echo '## Run `nix run .#makeBackendsDerivation` to update it.'
            } > "$path"/"$file".nix
            (cd "$path" || exit 1; cabal2nix . >> "$file".nix)
          }
          export -f runCabal2nix
          ## We use `bash -c` because shell functions can't be passed to
          ## external processes (`xargs` in that case).
          find . -name '*.cabal' | xargs -I{} bash -c 'runCabal2nix {}'
        '';
      };
      smtlib-backends = hpkgs.callPackage ./smtlib-backends.nix {};
      smtlib-backends-process = hpkgs.callPackage ./smtlib-backends-process/smtlib-backends-process.nix {
        inherit smtlib-backends smtlib-backends-tests;
      };
      smtlib-backends-tests = hpkgs.callPackage ./smtlib-backends-tests/smtlib-backends-tests.nix {inherit smtlib-backends;};
      smtlib-backends-z3 = hpkgs.callPackage ./smtlib-backends-z3/smtlib-backends-z3.nix {
        inherit smtlib-backends smtlib-backends-tests;
      };
    in {
      formatter = pkgs.alejandra;

      ## Generate derivations for Haskell packages.
      ## We use `cabal2nix` from shell rather than the Nix-level
      ## `callCabal2nix` to avoid using import from derivation.
      ## For motives, see https://github.com/NixOS/nix/pull/5253
      apps.makeBackendsDerivations = {
        type = "app";
        program = "${makeBackendsDerivations}/bin/makeBackendsDerivations";
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
