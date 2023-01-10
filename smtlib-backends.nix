## This file has been generated automatically.
## Run `nix run .#makeBackendsDerivation` to update it.
{ mkDerivation, base, bytestring, lib }:
mkDerivation {
  pname = "smtlib-backends";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [ base bytestring ];
  description = "Low-level functions for SMT-LIB-based interaction with SMT solvers";
  license = lib.licenses.mit;
}
