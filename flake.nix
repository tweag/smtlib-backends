{
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.nixpkgs.url = github:NixOS/nixpkgs;

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      cvc5_latest = pkgs.cvc5.overrideAttrs (old: {
        version = "2023-11-27";
        src = pkgs.fetchFromGitHub {
          owner  = "cvc5";
          repo   = "cvc5";
          rev    = "a33daf1b313f71332a00cc4556577962cc731bd2";
          hash  = "sha256-uKIKjsBs4neC2ZotOeyHO6ddY3CCqwoePB7gfvVogHY=";
        };
        nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.flex ];
        buildInputs = with pkgs; [
          cadical.dev symfpu gmp gtest libantlr3c antlr3_4 boost jdk
          (python3.withPackages (ps: with ps; [ pyparsing toml tomli ]))
        ];
      });
      hpkgs = pkgs.haskellPackages;
      smtlib-backends = hpkgs.callPackage ./smtlib-backends.nix {};
      smtlib-backends-process = hpkgs.callPackage ./smtlib-backends-process/smtlib-backends-process.nix {
        inherit smtlib-backends smtlib-backends-tests;
      };
      smtlib-backends-tests = hpkgs.callPackage ./smtlib-backends-tests/smtlib-backends-tests.nix {inherit smtlib-backends;};
      smtlib-backends-z3 = hpkgs.callPackage ./smtlib-backends-z3/smtlib-backends-z3.nix {
        inherit smtlib-backends smtlib-backends-tests;
      };
      smtlib-backends-cvc5 = hpkgs.callPackage ./smtlib-backends-cvc5/smtlib-backends-cvc5.nix {
        inherit smtlib-backends smtlib-backends-tests;
        cvc5 = cvc5_latest;
      };
    in {
      formatter = pkgs.alejandra;

      ## Generate derivations for Haskell packages.
      ## We use `cabal2nix` from shell rather than the Nix-level
      ## `callCabal2nix` to avoid using 'import from derivation'.
      ## For motives, see https://github.com/NixOS/nix/pull/5253
      apps.makeBackendsDerivations = let
        ## The script that generates a Nix derivation for each cabal
        ## file present in the repository.
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
            find . -name '*.cabal' -print0 | xargs --null -I{} bash -c 'runCabal2nix {}'
          '';
        };
      in {
        type = "app";
        program = "${makeBackendsDerivations}/bin/makeBackendsDerivations";
      };

      checks.formatting = pkgs.stdenv.mkDerivation {
        name = "smtlib-backends format";
        src = ./.;
        nativeBuildInputs = [pkgs.ormolu];
        buildPhase = ''
          ormolu --mode check $(find . -name '*.hs') || exit 1
        '';
        installPhase = "mkdir $out";
      };

      devShells = let
        ## Needed by Z3 tests and haskell language server
        LD_LIBRARY_PATH = with pkgs; lib.strings.makeLibraryPath [z3 cvc5_latest];
        packages = _: [
          smtlib-backends
          smtlib-backends-tests
          smtlib-backends-process
          smtlib-backends-z3
          smtlib-backends-cvc5
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
            ++ (with pkgs; [z3 cvc5_latest ormolu]);

          inherit LD_LIBRARY_PATH;
        };

        ## Lightweight development shell.
        lightweight = hpkgs.shellFor {
          buildInputs = with pkgs; [ghc cabal-install z3 cvc5_latest];
          inherit packages;
          inherit LD_LIBRARY_PATH;
        };
      };
    });
}
