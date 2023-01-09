## This file has been generated automatically.
## Run `nix run .#makeBackendsDerivation` to update it.
{ mkDerivation, async, base, bytestring, data-default, lib
, smtlib-backends, smtlib-backends-tests, tasty, tasty-hunit
, typed-process
}:
mkDerivation {
  pname = "smtlib-backends-process";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring data-default smtlib-backends typed-process
  ];
  testHaskellDepends = [
    base bytestring data-default smtlib-backends smtlib-backends-tests
    tasty tasty-hunit typed-process
  ];
  description = "An SMT-LIB backend running solvers as external processes";
  license = lib.licenses.mit;
}
