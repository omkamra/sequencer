# Change Log

## [Unreleased]

### Changed

- `:var` forms now check if the resolved var is a function, and if it
  is, apply it to the arguments following the var inside the `:var`
  form

## [0.2.0] - 2021-12-22

### Added

- `sequencer/compile`: a shorthand for `sequencer/compile-pattern-form`
- `sequencer/register-target` accepts an optional target alias (a
  namespace-qualified keyword) which can be used to refer to the
  target in `:bind` maps
- patterns got a `delay` value: when a pattern is merged onto the
  timeline, each event of the pattern is delayed by this number of
  ticks (used to make `:play` more precise)
- `[:delay N]`: this new pattern expression sets the delay value of
  the pattern to N

### Changed

- `:call` pattern expressions can now include function arguments
- `:var` pattern expressions now wrap the var value in a `:bind` form
  with the input bindings if it needs to be compiled (this ensures
  that they are compiled in the right context and target)

### Fixed

- sequencer crashes now properly reset sequencer state

## [0.1.0] - 2021-12-19

Initial release.

[Unreleased]: https://github.com/omkamra/sequencer/compare/0.2.0...HEAD
[0.2.0]: https://github.com/omkamra/sequencer/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/omkamra/sequencer/tree/0.1.0
