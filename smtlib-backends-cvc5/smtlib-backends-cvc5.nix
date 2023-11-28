## This file has been generated automatically.
## Run `nix run .#makeBackendsDerivation` to update it.
{ mkDerivation, base, bytestring, lib, smtlib-backends, smtlib-backends-tests, tasty
, tasty-hunit, cvc5
}:
mkDerivation {
  pname = "smtlib-backends-cvc5";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring smtlib-backends
  ];
  librarySystemDepends = [ cvc5 ];
  testHaskellDepends = [
    base bytestring smtlib-backends smtlib-backends-tests tasty
    tasty-hunit
  ];
  description = "An SMT-LIB backend implemented using CVC5's C API";
  license = lib.licenses.mit;
}
