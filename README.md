# SMT-LIB backends

This Haskell library provides different low-level ways of interacting with SMT
solvers using [SMT-LIB](https://smtlib.cs.uiowa.edu/).

We currently provide two different backends: a classic backend, available in the 
`smtlib-backends-process` package, implemented by running solvers as external processes, 
and a faster backend, available in the `smtlib-backends-z3` package, implemented using Z3
as a library.

In addition, the API allows for queuing commands so that they are sent to the backend 
only when a response is needed, as we have observed this to reduce the communication
overhead. See the documentation of
[SMTLIB.Backends.Solver](src/SMTLIB/Backends.hs) for the details.

## Usage

Here is an example, using GHCi, of how to communicate with the solver
Yices2 as an external process.
It requires the libraries `smtlib-backends` and `smtlib-backends-process`.
```haskell
> import qualified SMTLIB.Backends.Process as P
> import qualified SMTLIB.Backends as SMT
> :set -XOverloadedStrings
> let cfg = P.defaultConfig { P.exe = "yices-smt2", P.args = [] }
> yicesProcess <- P.new cfg
> yices <- SMT.initSolver SMT.NoQueuing (P.toBackend yicesProcess)
> SMT.command yices "(get-info :name)" >>= print
"(:name \"Yices\")"
```

More examples of how to use the different backends are included in their
respective test-suites:
- [examples for the `Process` backend](smtlib-backends-process/tests/Examples.hs)
- [examples for the `Z3` backend](smtlib-backends-z3/tests/Examples.hs)

## Building and testing

This repository provides a reproducible build environment through a [Nix
flake](https://www.tweag.io/blog/2020-05-25-flakes/).

Another option is to manually install Z3 to use the process backend, and the
Z3 C library to use the Z3 backend. Then you can build and test the libraries
using `cabal build` and `cabal test`.

## Implementing backends

Currently, backends only need to provide a function to submit queries, as
documented in [SMTLIB.Backends.Backend](src/SMTLIB/Backends.hs). See
[SMTLIB.Backends.Process.toBackend](smtlib-backends-process/src/SMTLIB/Backends/Process.hs) or
[SMTLIB.Backends.Z3.toBackend](smtlib-backends-z3/src/SMTLIB/Backends/Z3.hs) for examples.

## Motivation

This library was created because there are a lot of Haskell projects using SMT solvers
through SMT-LIB, but most of them only use solvers through external processes
and implement the interaction with the solver themselves. But running solvers
as external processes can be quite slow, hence this library aims to provide
other, more efficient ways to do so. We believe having one well-optimized and
safe library is more efficient than having the same code be spread out between
different projects.

## Contributing

### Code formatting

We format our code using [ormolu](https://github.com/tweag/ormolu) (more specifically the version from [NixOS/nixpkgs](https://github.com/NixOS/nixpkgs)'s master branch). The `.cabal` files are formatted using [`cabal-fmt`](https://github.com/phadej/cabal-fmt). It is thus recommended to add the following script as your `.git/hooks/pre-commit`:
```bash
#!/usr/bin/env bash
set -e

# command adapted from https://github.com/JLLeitschuh/ktlint-gradle  task addKtlintFormatGitPreCommitHook
filesToFormat="$(git --no-pager diff --name-status --no-color --cached | \
  awk '($1 == "M" || $1 == "A") && $2 ~ /\.hs/ { print $2} $1 ~ /R/ && $3 ~ /\.hs/ { print $3 } ')"

echo "files to format $filesToFormat"
for sourceFilePath in $filesToFormat
do
  ormolu --mode inplace "$(pwd)/$sourceFilePath"
  git add $sourceFilePath
done;
cabal-fmt --inplace $(find . -name '*.cabal')
```
