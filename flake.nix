{
  inputs.flake-utils.url = github:numtide/flake-utils;

  nixConfig = {
    # Needed by callCabal2nix
    allow-import-from-derivation = true;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    with flake-utils.lib;
      eachSystem [system.x86_64-linux] (system: let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;
        smtlib-backends = hpkgs.callCabal2nix "smtlib-backends" ./. {};
        smtlib-backends-process = hpkgs.callCabal2nix "smtlib-backends-process" ./smtlib-backends-process {inherit smtlib-backends smtlib-backends-tests;};
        smtlib-backends-tests = hpkgs.callCabal2nix "smtlib-backends-tests" ./smtlib-backends-tests {inherit smtlib-backends;};
        smtlib-backends-z3 = hpkgs.callCabal2nix "smtlib-backends-z3" ./smtlib-backends-z3 {
          inherit smtlib-backends smtlib-backends-tests;
        };
      in {
        formatter = pkgs.alejandra;

        devShells = let
          ## Needed by Z3 tests and haskell language server
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.z3.lib}/lib:''${LD_LIBRARY_PATH:+:}"
          '';
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

            inherit shellHook;
          };

          ## Lightweight development shell.
          lightweight = hpkgs.shellFor {
            buildInputs = with pkgs; [ghc cabal-install z3];
            inherit packages;
            inherit shellHook;
          };
        };
      });
}
