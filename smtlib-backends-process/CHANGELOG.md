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
