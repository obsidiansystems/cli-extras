{-|
Description: Convenience functions for writing CLI applications
|-}
module Cli.Extras
  (
  -- .Types
    CliLog
  , CliThrow
  , CliT(..)
  , runCli
  , CliConfig
  , HasCliConfig
  , getCliConfig
  , Output

  -- .Spinner
  , withSpinner
  , withSpinnerNoTrail
  , withSpinner'

  -- .Logging
  , AsUnstructuredError (..)
  , newCliConfig
  , getLogLevel
  , putLog
  , failWith
  , errorToWarning
  , withExitFailMessage

  -- Control.Monad.Log
  , Severity (..)

  -- .Process
  , AsProcessFailure (..)
  , ProcessFailure (..)
  , ProcessSpec (..)
  , callCommand
  , callProcess
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  , exitCodeToException
  , overCreateProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessAndLogOutput
  , readProcessAndLogStderr
  , readProcessJSONAndLogStderr
  , reconstructCommand
  , runProcess_
  , setCwd
  , setDelegateCtlc
  , setEnvOverride
  , shell
  , waitForProcess
  ) where

import Control.Monad.Log (Severity (..))

import Cli.Extras.Logging
import Cli.Extras.Process
import Cli.Extras.Spinner
import Cli.Extras.Types
