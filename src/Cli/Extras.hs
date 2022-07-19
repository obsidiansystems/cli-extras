{-|
Description: Convenience functions for writing CLI applications
|-}
module Cli.Extras
  (
  -- .Types
    CommandLineLog
  , CommandLineThrow
  , CommandLineT(..)
  , runCommandLine
  , CommandLineConfig
  , HasCommandLineConfig
  , getCommandLineConfig
  , Output

  -- .Spinner
  , withSpinner
  , withSpinnerNoTrail
  , withSpinner'

  -- .Logging
  , AsUnstructuredError (..)
  , newCommandLineConfig
  , mkDefaultCommandLineConfig
  , getLogLevel
  , putLog
  , failWith
  , withExitFailMessage

  -- Control.Monad.Log
  , Severity (..)

  -- .Process
  , AsProcessFailure (..)
  , ProcessFailure (..)
  , prettyProcessFailure
  , ProcessSpec (..)
  , callCommand
  , callProcess
  , callProcessAndLogOutput
  , createProcess_
  , overCreateProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessAndLogOutput
  , readProcessAndLogStderr
  , readProcessJSONAndLogStderr
  , reconstructCommand
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
