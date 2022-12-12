# SMT-LIB backends

This Haskell library provides different low-level ways of interacting with SMT
solvers using [SMT-LIB](https://smtlib.cs.uiowa.edu/).

It was created because there are a lot of Haskell projects using SMT solvers
through SMT-LIB, but most of them only use solvers through external processes
and implement the interaction with the solver themselves. But running solvers
as external processes can be quite slow, hence this library aims to provide
other, more efficient ways to do so. We believe having one well-optimized and
safe library is more efficient than having the same code be spread out between
different projects.

We currently provide two different backends: a classic backend implemented by
running solvers as external processes, and a faster backend, available in the
`smtlib-backends-z3` package, implemented using inlined calls to Z3's C API.

# Building and testing

This repository provides a reproducible build environment through a [Nix
flake](https://www.tweag.io/blog/2020-05-25-flakes/). If this is not an option
for you, you'll have to get a copy of Z3 for testing `smtlib-backends` and of
the Z3 C library for building `smtlib-backends-z3`.

You can then build and test the libraries using `cabal build` and `cabal test`.

# Contributing

## Code formatting

We format our code using [ormolu](https://github.com/tweag/ormolu) (more specifically the version from [NixOS/nixpkgs](https://github.com/NixOS/nixpkgs)'s master branch). It is thus recommended to add the following script as your `.git/hooks/pre-commit`:
```
#!/usr/bin/env bash
set -e

# command adapted from https://github.com/JLLeitschuh/ktlint-gradle  task addKtlintFormatGitPreCommitHook
filesToFormat="$(git --no-pager diff --name-status --no-color --cached | \
  awk '($1 == "M" || $1 == "A") && $2 ~ /\.hs/ { print $2} $1 ~ /R/ && $3 ~ /\.hs/ { print $3 } ')"

echo "files to format $filesToFormat"
for sourceFilePath in $filesToFormat
do
  ormolu --command ormolu --mode inplace "$(pwd)/$sourceFilePath"
  git add $sourceFilePath
done;
```

## Implementing backends

For our implementation of backends, we use and recommend using the [Handle
design pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html#the-module-layout).
