see also the changelogs of `smtlib-backends-tests`, `smtlib-backends-process` and
`smtlib-backends-z3`

# v0.3-alpha
- **(breaking change)** add a datatype `Backends.QueuingFlag` to set the queuing mode
  - the `initSolver` function now takes this datatype as argument instead of a
    boolean
- **(breaking change)** make the queuing functions thread-unsafe but faster
- add a `send_` method to the `Backends.Backend` datatype for sending commands with no output
- add a `Backends.flushQueue` function for forcing the content of the queue to be
  evaluated

# v0.2
- split the `Process` module into its own library
- rename `SMTLIB.Backends`'s `ackCommand` to `command_`
- remove logging abilities
  - the user can always surround `command` or `command_` with their own logging
    functions
- improve read-me
