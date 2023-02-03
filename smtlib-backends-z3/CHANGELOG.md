# Changelog

All notable changes to the smtlib-backends-z3 library will be documented in this
file.

## v0.3 _(2023-02-03)_

### Added
- add `Z3.defaultConfig`, synonym for `def`
- add tests for documenting edge cases of the backends
  - what happens when sending an empty command
  - what happens when sending a command not producing any output
- **(breaking change)** removed `Data.Default` instance of `Config`

### Changed
- make test-suite compatible with `smtlib-backends-0.3`
- **(breaking change)** the `Z3.new` and `Z3.with` functions now take a
  `Z3.Config` object as argument, which one may use to set some solver options
  at initialization time
  - add corresponding examples in the test-suite
- dropped dependency on `inline-c`

## v0.2 _(2022-12-16)_

### Added
- add usage examples in the test-suite

### Changed
- make test-suite compatible with `smtlib-backends-0.2`
