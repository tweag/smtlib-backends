# SMT-LIB backends

This Haskell library provides different low-level ways to interact with SMT solvers using [SMT-LIB](https://smtlib.cs.uiowa.edu/).

It was created because there are a lot of Haskell projects using SMT solvers through SMT-LIB, but most of them only use solvers through external processes and implement the interaction with the solver themselves. But running solvers as external processes can be quite slow, hence this library aims to provide other, more efficient ways to do so.

We currently provide two different backends: a classic backend implemented by running solvers as external processes, and a faster backend, available in the `smtlib-backends-z3` package, implemented using inlined calls to Z3's C API.
