{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Cli.Extras.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT(..), MonadLog, Severity (..), WithSeverity (..))
import Control.Monad.Reader (MonadIO, ReaderT (..), MonadReader (..), ask)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Trans (MonadTrans, lift)
import Data.IORef (IORef)
import Data.Text (Text)

import Cli.Extras.TerminalString (TerminalString)
import Cli.Extras.Theme (CommandLineTheme)
import Cli.Extras.SubExcept

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail
#endif

--------------------------------------------------------------------------------

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Write [TerminalString]  -- Render and write a TerminalString using putstrLn
  | Output_Overwrite [TerminalString]  -- Overwrite the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clear the line
  deriving (Eq, Show, Ord)

type CommandLineLog m = MonadLog Output m

type CommandLineThrow e m = MonadError e m

--------------------------------------------------------------------------------

data CommandLineConfig = CommandLineConfig
  { -- | We are capable of changing the log level at runtime
    _commandLineConfig_logLevel :: IORef Severity
  , -- | Disallow coloured output
    _commandLineConfig_noColor :: Bool
  , -- | Disallow spinners
    _commandLineConfig_noSpinner :: Bool
  , -- | Whether the last message was an Overwrite output
    _commandLineConfig_lock :: MVar Bool
  , -- | Whether the user tip (to make verbose) was already displayed
    _commandLineConfig_tipDisplayed :: IORef Bool
  , -- | Stack of logs from nested spinners
    _commandLineConfig_spinnerStack :: IORef ([Bool], [TerminalString])
  , -- | Theme strings for spinners
    _commandLineConfig_theme :: CommandLineTheme
  }

class Monad m => HasCommandLineConfig m where
  getCommandLineConfig :: m CommandLineConfig

instance HasCommandLineConfig m => HasCommandLineConfig (ReaderT r m) where
  getCommandLineConfig = lift getCommandLineConfig

instance (Monoid w, HasCommandLineConfig m) => HasCommandLineConfig (WriterT w m) where
  getCommandLineConfig = lift getCommandLineConfig

instance HasCommandLineConfig m => HasCommandLineConfig (StateT s m) where
  getCommandLineConfig = lift getCommandLineConfig

instance HasCommandLineConfig m => HasCommandLineConfig (ExceptT e m) where
  getCommandLineConfig = lift getCommandLineConfig

instance HasCommandLineConfig m => HasCommandLineConfig (SubExceptT e eSub m) where
  getCommandLineConfig = lift getCommandLineConfig

--------------------------------------------------------------------------------

newtype CommandLineT e m a = CommandLineT
  { unCommandLineT :: ReaderT CommandLineConfig (LoggingT Output (ExceptT e m)) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadFail
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output -- CommandLineLog
    , MonadError e -- CommandLineThrow
    )

instance MonadTrans (CommandLineT e) where
  lift = CommandLineT . lift . lift . lift

instance Monad m => HasCommandLineConfig (CommandLineT e m)where
  getCommandLineConfig = CommandLineT ask
