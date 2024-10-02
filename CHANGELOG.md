# Revision history for cli-extras

## 0.2.1.0 -- revision 2

* Support GHC 9.10

## 0.2.1.0 -- revision 1

* Support GHC 9.8

## 0.2.1.0

* [#7](https://github.com/obsidiansystems/cli-extras/pull/7) add `runProc`, `runProcSilently` and `readProc`.

## 0.2.0.0

* [#6](https://github.com/obsidiansystems/cli-extras/pull/6) `cli-extras` now depends on `utf8-string` and `shell-escape`
* [#6](https://github.com/obsidiansystems/cli-extras/pull/6) Removed `mkDefaultCliConfig`, `prettyProcessFailure`
* [#6](https://github.com/obsidiansystems/cli-extras/pull/6) `cli-extras` now manages the control flow for exceptions:
  + The type `CliConfig` is now parametrised over the type of errors, and the class `HasCliConfig` was changed accordingly
  + The return type of `runCli` was changed from `m (Either e a)` to `m a`.
  + A new transformer `DieT` was added as an intermediate between the full `CliT` and "`LoggingT` with errors". Errors thrown in `DieT` can not be caught, and abort the program according to the configuration.
  * `newCliConfig` now takes an argument describing how errors should be handled.
* [#6](https://github.com/obsidiansystems/cli-extras/pull/6) Added helper functions:
  + `runProcess_` (run a `ProcessSpec`, aborting if the called process fails)
  + `fork` (lifts `forkIO` to the `CliT` monad)
  + `whenLogLevel` (runs an action iff the current log level matches a predicate)
  + `errorToWarning` (log an error as if it were a warning)
* [#6](https://github.com/obsidiansystems/cli-extras/pull/6) The action `allowUserToMakeLoggingVerbose` now takes an argument specifying how the key combination should be shown to the user. Additionally, the `CliConfig` argument was removed and made into a `HasCliConfig` constraint.

## 0.1.0.2

* Loosen aeson, which, ansi-terminal version bounds

## 0.1.0.1

* Loosen version bounds. Support GHC 8.8.4.
* Remove orphan MonadFail instance for LoggingT (we now require monad-logger >= 0.3.30)

## 0.1.0.0
* Initial release
