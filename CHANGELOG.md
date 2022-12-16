see also the changelogs of `smtlib-backends-tests`, `smtlib-backends-process` and 
`smtlib-backends-z3`

# v0.2
- split the `Process` module into its own library
- rename `SMTLIB.Backends`'s `ackCommand` to `command_`
- remove logging abilities
  - the user can always surround `command` or `command_` with their own logging
    functions
- improve read-me

