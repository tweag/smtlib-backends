{ mkDerivation, base, lib, smtlib-backends, tasty, tasty-hunit }:
mkDerivation {
  pname = "smtlib-backends-tests";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [ base smtlib-backends tasty tasty-hunit ];
  description = "Testing SMT-LIB backends";
  license = lib.licenses.mit;
}
