# v0.3-alpha
- make test-suite compatible with `smtlib-backends-0.3`
- **(breaking change)** the `Z3.new` and `Z3.with` functions now take a `Z3.Config`
object as argument, which one may use to set some solver options at initialization
time
  - add corresponding examples in the test-suite
- add `Z3.defaultConfig`, synonym for `def`
- add tests for documenting edge cases of the backends
  - what happens when sending an empty command
  - what happens when sending a command not producing any output
- **(breaking change)** removed `Data.Default` instance of `Config`
- dropped dependency on `inline-c`

# v0.2
- make test-suite compatible with `smtlib-backends-0.2`
- add usage examples in the test-suite
