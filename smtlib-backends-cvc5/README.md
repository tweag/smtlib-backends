# SMT-LIB CVC5 experimental backend

__This library is currently EXPERIMENTAL__

Like the `smtlib-backends-z3` package, this package uses an experimental version
of CVC5 as a library, which allows parsing and executing SMT commands


This Haskell library provides different low-level ways of interacting with SMT
solvers using [SMT-LIB](https://smtlib.cs.uiowa.edu/).

This libary is currently experimental as it requires the latest version of CVC5
(github commit a33daf1b313f71332a00cc4556577962cc731bd2 as of writing) which exports
a CVC5's SMTLIB parser as an API. 
`smtlib-backends-cvc5 has also only been tested on GHC 9.4, due to this bug on macOS 
https://gitlab.haskell.org/ghc/ghc/-/issues/11829, which prevents C++ errors from being 
caught when used within a Haskell binary.

Note that using this library will also requite using `clang++`/`g++` as linker to build 
your final Haskell binary. See the `test-suite`'s `ghc-options` and `ld-options` in 
`smtlib-backends-cvc5.cabal` for details.