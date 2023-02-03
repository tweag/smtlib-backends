## This file has been generated automatically.
## Run `nix run .#makeBackendsDerivation` to update it.
{ mkDerivation, base, bytestring, containers, data-default, gomp
, inline-c, lib, smtlib-backends, smtlib-backends-tests, tasty
, tasty-hunit, z3
}:
mkDerivation {
  pname = "smtlib-backends-z3";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers data-default inline-c smtlib-backends
  ];
  librarySystemDepends = [ gomp z3 ];
  testHaskellDepends = [
    base bytestring smtlib-backends smtlib-backends-tests tasty
    tasty-hunit
  ];
  description = "An SMT-LIB backend implemented using Z3's C API";
  license = lib.licenses.mit;
}
