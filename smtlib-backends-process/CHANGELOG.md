# Changelog

All notable changes to the smtlib-backends-process library will be documented in
this file.

## v0.3

### Added
- add tests for documenting edge cases of the backends
  - check that we can pile up procedures for exiting a process
  - what happens when sending an empty command
  - what happens when sending a command not producing any output
- add `Process.defaultConfig`, synonym for `def`

### Changed
- make the test-suite compatible with `smtlib-backends-0.3`
- **(breaking change)** rename `Process.close` to `Process.kill`
- **(breaking change)** rename `Process.wait` to `Process.close` and improve it
  - ensure the process gets killed even if an error is caught
  - send an `(exit)` command before waiting for the process to exit
  - this means `Process.with` now closes the process with this new version of
    `Process.close`, hence gracefully
- improve error messages inside `Process.toBackend`

## v0.2 _(2022-12-16)_

### Added
- made `Config` an instance of the `Default` typeclass
- add usage examples in the test-suite

### Changed
- split `smtlib-backends`'s `Process` module into its own library
- move the logger function inside the `Config` datatype
- don't prefix error messages with `[stderr]`
- make compatible with `smtlib-backends-0.2`
- improve documentation
