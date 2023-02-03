## This file has been generated automatically.
## Run `nix run .#makeBackendsDerivation` to update it.
{ mkDerivation, async, base, bytestring, lib, process
, smtlib-backends, smtlib-backends-tests, tasty, tasty-hunit
}:
mkDerivation {
  pname = "smtlib-backends-process";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring process smtlib-backends
  ];
  testHaskellDepends = [
    async base bytestring process smtlib-backends smtlib-backends-tests
    tasty tasty-hunit
  ];
  description = "An SMT-LIB backend running solvers as external processes";
  license = lib.licenses.mit;
}
