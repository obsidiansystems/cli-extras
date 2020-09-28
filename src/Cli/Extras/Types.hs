{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Cli.Extras.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Log (LoggingT(..), MonadLog, Severity (..), WithSeverity (..))
import Control.Monad.Reader (MonadIO, ReaderT (..), MonadReader (..), ask)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Trans (MonadTrans, lift)
import Data.IORef (IORef)
import Data.Text (Text)

import Cli.Extras.TerminalString (TerminalString)
import Cli.Extras.Theme (CliTheme)
import Cli.Extras.SubExcept

--------------------------------------------------------------------------------

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Write [TerminalString]  -- Render and write a TerminalString using putstrLn
  | Output_Overwrite [TerminalString]  -- Overwrite the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clear the line
  deriving (Eq, Show, Ord)

type CliLog m = MonadLog Output m

type CliThrow e m = MonadError e m

deriving instance MonadFail m => MonadFail (LoggingT Output m)

--------------------------------------------------------------------------------

data CliConfig = CliConfig
  { -- | We are capable of changing the log level at runtime
    _cliConfig_logLevel :: IORef Severity
  , -- | Disallow coloured output
    _cliConfig_noColor :: Bool
  , -- | Disallow spinners
    _cliConfig_noSpinner :: Bool
  , -- | Whether the last message was an Overwrite output
    _cliConfig_lock :: MVar Bool
  , -- | Whether the user tip (to make verbose) was already displayed
    _cliConfig_tipDisplayed :: IORef Bool
  , -- | Stack of logs from nested spinners
    _cliConfig_spinnerStack :: IORef ([Bool], [TerminalString])
  , -- | Theme strings for spinners
    _cliConfig_theme :: CliTheme
  }

class Monad m => HasCliConfig m where
  getCliConfig :: m CliConfig

instance HasCliConfig m => HasCliConfig (ReaderT r m) where
  getCliConfig = lift getCliConfig

instance (Monoid w, HasCliConfig m) => HasCliConfig (WriterT w m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig m => HasCliConfig (StateT s m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig m => HasCliConfig (ExceptT e m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig m => HasCliConfig (SubExceptT e eSub m) where
  getCliConfig = lift getCliConfig

--------------------------------------------------------------------------------

newtype CliT e m a = CliT
  { unCliT :: ReaderT CliConfig (LoggingT Output (ExceptT e m)) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadFail
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output -- CliLog
    , MonadError e -- CliThrow
    )

instance MonadTrans (CliT e) where
  lift = CliT . lift . lift . lift

instance Monad m => HasCliConfig (CliT e m)where
  getCliConfig = CliT ask
