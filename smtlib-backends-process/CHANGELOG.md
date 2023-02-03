# v0.3-alpha
- make test-suite compatible with `smtlib-backends-0.3`
- add tests for documenting edge cases of the backends
  - check that we can pile up procedures for exiting a process
  - what happens when sending an empty command
  - what happens when sending a command not producing any output
- add `Process.defaultConfig`, synonym for `def`
- improve error messages inside `Process.toBackend`
- **(breaking change)** use `process` instead of `typed-process` to manage the underlying process
  - change the definition of the `Process.Handle` datatype accordingly
  - remove `Process.wait`
  - there is now a single example in the test-suite showing how to 
    manage the underlying process and its I/O channels
- **(breaking change)** removed logging capabilities, this is now on the user to
  implement
  - remove `Config`'s `reportError` field
  - remove `Handle`'s `errorReader` field
- **(breaking change)** removed `Data.Default` instance of `Config`

# v0.2
split `smtlib-backends`'s `Process` module into its own library
## `Config` datatype
- move the logger function into it
- make it an instance of the `Default` typeclass
## logging
- move the logger function into the `Config` datatype
- don't prefix error messages with `[stderr]`
## test-suite
- add usage examples
- make compatible with `smtlib-backends-0.2`
## miscellaneous
- improve documentation
