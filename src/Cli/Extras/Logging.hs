{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides a logging handler that facilitates safe ouputting to terminal using MVar based locking.
-- | Spinner.hs and Process.hs work on this guarantee.
module Cli.Extras.Logging
  ( AsUnstructuredError (..)
  , newCliConfig
  , runCli
  , verboseLogLevel
  , isOverwrite
  , getSeverity
  , getLogLevel
  , setLogLevel
  , putLog
  , putLogRaw
  , failWith
  , errorToWarning
  , withExitFailMessage
  , writeLog
  , allowUserToMakeLoggingVerbose
  , getChars
  , fork
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Lens (Prism', review)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask, bracket, catch, throwM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (Severity (..), WithSeverity (..), logMessage, runLoggingT)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Reader (MonadIO, ReaderT (..))
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Encoding.Types
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleIntensity (FaintIntensity), ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity), clearLine)
import System.Exit (ExitCode (..))
import System.IO

import qualified Cli.Extras.TerminalString as TS
import Cli.Extras.Theme
import Cli.Extras.Types

-- | Create a new 'CliConfig', initialized with the provided values.
newCliConfig
  :: Severity
  -- ^ The initial log level. Messages below this severity will not be
  -- logged, unless the log level is subsequently altered using
  -- 'setLogLevel'.
  -> Bool -- ^ Should ANSI terminal formatting be disabled?
  -> Bool -- ^ Should spinners be disabled?
  -> (e -> (Text, ExitCode))
  -- ^ How to display errors, and compute the 'ExitCode' corresponding
  -- to each error.
  -> IO (CliConfig e)
newCliConfig sev noColor noSpinner errorLogExitCode = do
  level <- newIORef sev
  lock <- newMVar False
  tipDisplayed <- newIORef False
  stack <- newIORef ([], [])
  textEncoding <- hGetEncoding stdout
  let theme = if maybe False supportsUnicode textEncoding
        then unicodeTheme
        else noUnicodeTheme
  return $ CliConfig level noColor noSpinner lock tipDisplayed stack errorLogExitCode theme

runCli :: MonadIO m => CliConfig e -> CliT e m a -> m a
runCli c =
    flip runLoggingT (handleLog c)
  . flip runReaderT (_cliConfig_errorLogExitCode c)
  . unDieT
  . flip runReaderT c
  . unCliT

verboseLogLevel :: Severity
verboseLogLevel = Debug

isOverwrite :: Output -> Bool
isOverwrite = \case
  Output_Overwrite _ -> True
  _ -> False

getSeverity :: Output -> Maybe Severity
getSeverity = \case
  Output_Log (WithSeverity sev _) -> Just sev
  Output_LogRaw (WithSeverity sev _) -> Just sev
  _ -> Nothing

getLogLevel :: (MonadIO m, HasCliConfig e m) => m Severity
getLogLevel = getLogLevel' =<< getCliConfig

getLogLevel' :: MonadIO m => CliConfig e -> m Severity
getLogLevel' = liftIO . readIORef . _cliConfig_logLevel

setLogLevel :: (MonadIO m, HasCliConfig e m) => Severity -> m ()
setLogLevel sev = do
  l <- _cliConfig_logLevel <$> getCliConfig
  liftIO $ writeIORef l sev

handleLog :: MonadIO m => CliConfig e -> Output -> m ()
handleLog conf output = do
  level <- getLogLevel' conf
  liftIO $ modifyMVar_ (_cliConfig_lock conf) $ \wasOverwriting -> do
    let noColor = _cliConfig_noColor conf
    case getSeverity output of
      Nothing -> handleLog' noColor output
      Just sev -> if sev > level
        then return wasOverwriting  -- Discard if sev is above configured log level
        else do
          -- If the last output was an overwrite (with cursor on same line), ...
          when wasOverwriting $
            void $ handleLog' noColor Output_ClearLine  -- first clear it,
          handleLog' noColor output  -- then, actually write the msg.

handleLog' :: MonadIO m => Bool -> Output -> m Bool
handleLog' noColor output = do
  case output of
    Output_Log m -> liftIO $ do
      writeLog True noColor m
    Output_LogRaw m -> liftIO $ do
      writeLog False noColor m
      hFlush stdout  -- Explicitly flush, as there is no newline
    Output_Write ts -> liftIO $ do
      T.putStrLn $ TS.render (not noColor) Nothing ts
      hFlush stdout
    Output_Overwrite ts -> liftIO $ do
      width <- TS.getTerminalWidth
      T.putStr $ "\r" <> TS.render (not noColor) width ts
      hFlush stdout
    Output_ClearLine -> liftIO $ do
      -- Go to the first column and clear the whole line
      putStr "\r"
      clearLine
      hFlush stdout
  return $ isOverwrite output

-- | Like `putLog` but without the implicit newline added.
putLogRaw :: CliLog m => Severity -> Text -> m ()
putLogRaw sev = logMessage . Output_LogRaw . WithSeverity sev

-- | Indicates unstructured errors form one variant (or conceptual projection)
-- of the error type.
--
-- Shouldn't really use this, but who has time to clean up that much!
class AsUnstructuredError e where
  asUnstructuredError :: Prism' e Text

instance AsUnstructuredError Text where
  asUnstructuredError = id

-- | Like `putLog Alert` but also abrupts the program.
failWith :: (CliThrow e m, AsUnstructuredError e) => Text -> m a
failWith = throwError . review asUnstructuredError

-- | Log an error as though it were a warning, in a non-fatal way.
errorToWarning
  :: (HasCliConfig e m, CliLog m)
  => e -> m ()
errorToWarning e = do
  c <- getCliConfig
  putLog Warning $ fst $ _cliConfig_errorLogExitCode c e

-- | Intercept ExitFailure exceptions and log the given alert before exiting.
--
-- This is useful when you want to provide contextual information to a deeper failure.
withExitFailMessage :: (CliLog m, MonadCatch m) => Text -> m a -> m a
withExitFailMessage msg f = f `catch` \(e :: ExitCode) -> do
  case e of
    ExitFailure _ -> putLog Alert msg
    ExitSuccess -> pure ()
  throwM e

-- | Log a message to standard output.
writeLog
  :: (MonadIO m)
  => Bool -- ^ Should a new line be printed after the message?
  -> Bool -- ^ Should ANSI terminal formatting be used when printing the message?
  -> WithSeverity Text -- ^ The message to print.
  -> m ()
writeLog withNewLine noColor (WithSeverity severity s) = if T.null s then pure () else write
  where
    write
      | noColor && severity <= Warning = liftIO $ putFn $ T.pack (show severity) <> ": " <> s
      | not noColor && severity <= Error = TS.putStrWithSGR errorColors h withNewLine s
      | not noColor && severity <= Warning = TS.putStrWithSGR warningColors h withNewLine s
      | not noColor && severity == Notice = TS.putStrWithSGR noticeColors h withNewLine s
      | not noColor && severity == Informational = TS.putStrWithSGR infoColors h withNewLine s
      | not noColor && severity >= Debug = TS.putStrWithSGR debugColors h withNewLine s
      | otherwise = liftIO $ putFn s

    putFn = if withNewLine then T.hPutStrLn h else T.hPutStr h
    h = if severity <= Error then stderr else stdout
    errorColors = [SetColor Foreground Vivid Red]
    warningColors = [SetColor Foreground Vivid Yellow]
    infoColors = [SetColor Foreground Vivid Green]
    noticeColors = [SetColor Foreground Vivid Blue]
    debugColors = [SetConsoleIntensity FaintIntensity]

-- | Runs an action only when the current log level matches a given
-- predicate.
whenLogLevel
  :: (MonadIO m, HasCliConfig e m)
  => (Severity -> Bool) -- ^ What severity(ies) should this action run in?
  -> m ()               -- ^ The action to run.
  -> m ()
whenLogLevel level f = do
  l <- getLogLevel
  when (level l) f

-- | Allows the user to immediately switch to verbose logging when a
-- particular sequence of characters is read from the terminal.
--
-- Call this function in a thread, and kill it to turn off keystroke monitoring.
allowUserToMakeLoggingVerbose
  :: (MonadIO m, MonadMask m, CliLog m, HasCliConfig e m)
  => String  -- ^ The key(s) which should be read to indicate a shift in verbosity.
  -> Text    -- ^ A description of the key that must be pressed.
  -> m ()
allowUserToMakeLoggingVerbose keyCode desc = bracket showTip (liftIO . killThread) $ \_ -> do
  whenLogLevel (/= verboseLogLevel) $ do
    liftIO $ hSetBuffering stdin NoBuffering
    _ <- iterateUntil (== keyCode) $ liftIO getChars
    putLog Warning $ desc <> " pressed; making output verbose (-v)"
    setLogLevel verboseLogLevel
  where
    showTip = fork $ whenLogLevel (/= verboseLogLevel) $ do
      conf <- getCliConfig
      liftIO $ threadDelay $ 10*1000000  -- Only show tip for actions taking too long (10 seconds or more)
      tipDisplayed <- liftIO $ atomicModifyIORef' (_cliConfig_tipDisplayed conf) $ (,) True
      unless tipDisplayed $ whenLogLevel (/= verboseLogLevel) $ do -- Check again in case the user had pressed Ctrl+e recently
        putLog Notice $ "Tip: Press " <> desc <> " to display full output"

-- | Like `getChar` but also retrieves the subsequently pressed keys.
--
-- Allowing, for example, the ↑ key, which consists of the three characters
-- ['\ESC','[','A'] to be distinguished from an actual \ESC character input.
getChars :: IO String
getChars = reverse <$> f mempty
  where
    f xs = do
      x <- getChar
      hReady stdin >>= \case
        True -> f (x:xs)
        False -> return (x:xs)

-- | Fork a computation in 'CliT', sharing the configuration with the
-- child thread.
fork :: (HasCliConfig e m, MonadIO m) => CliT e IO () -> m ThreadId
fork f = do
  c <- getCliConfig
  liftIO $ forkIO $ runCli c f

-- | Conservatively determines whether the encoding supports Unicode.
--
-- Currently this uses a whitelist of known-to-work encodings. In principle it
-- could test dynamically by opening a file with this encoding, but it doesn't
-- look like base exposes any way to determine this in a pure fashion.
supportsUnicode :: TextEncoding -> Bool
supportsUnicode enc = any ((textEncodingName enc ==) . textEncodingName)
  [ utf8
  , utf8_bom
  , utf16
  , utf16be
  , utf16le
  , utf32
  , utf32be
  , utf32le
  ]
