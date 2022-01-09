# Change Log

## [0.4.1] - 2022-01-09

### Fixed

- an embarrassing bug in `nanosleep` which nullified all attempts at
  adapting to the measured nanosleep/parknanos precision (the fact
  that I did not detect this bug makes me wonder if the whole idea is
  a premature and unnecessary optimization)

## [0.4.0] - 2022-01-05

### Changed

- the task of waiting until the next tick has been abstracted into a
  `Ticker` protocol with two implementations: `SleepingTicker` uses
  various sleep methods, `AudioSyncingTicker` synchronizes the ticks
  to audio-in - unfortunately the latter takes too much CPU in its
  current form so `SleepingTicker` remains the default

### Fixed

- 100% CPU usage due to the precision of nanosleep/parknanos becoming
  too large

## [0.3.0] - 2021-12-24

### Added

- a section to the README which describes the pattern lifecycle
- some tests

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

[0.4.1]: https://github.com/omkamra/sequencer/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/omkamra/sequencer/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/omkamra/sequencer/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/omkamra/sequencer/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/omkamra/sequencer/tree/0.1.0
