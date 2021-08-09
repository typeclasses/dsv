# Changelog

## v1.0.0.0 - October 20, 2020

Initial version

## v1.0.0.1 - March 9, 2021

Support for GHC 9.0, although the test suite doesn't run yet;
waiting for other dependencies to support GHC 9.0 as well.

Support `bytestring-0.11`

Remove the `doctest` test suite; moved everything into the
`hedgehog` suite.

## v1.0.0.2 - August 9, 2021

Rename the test executable to support case-insensitive file systems
