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
  , mkDefaultCliConfig
  , runCli
  , verboseLogLevel
  , isOverwrite
  , getSeverity
  , getLogLevel
  , setLogLevel
  , putLog
  , putLogRaw
  , failWith
  , withExitFailMessage
  , writeLog
  , allowUserToMakeLoggingVerbose
  , getChars
  , handleLog
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Lens (Prism', review)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask, bracket, catch, throwM)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (Severity (..), WithSeverity (..), logMessage, runLoggingT)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Reader (MonadIO, ReaderT (..))
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Encoding.Types
import System.Console.ANSI (Color (Red, Yellow), ColorIntensity (Vivid),
                            ConsoleIntensity (FaintIntensity), ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity), clearLine)
import System.Environment
import System.Exit (ExitCode (..))
import System.IO

import qualified Cli.Extras.TerminalString as TS
import Cli.Extras.Theme
import Cli.Extras.Types

-- | Log a message to the console.
--
-- Logs safely even if there are ongoing spinners.
putLog :: CliLog m => Severity -> Text -> m ()
putLog sev = logMessage . Output_Log . WithSeverity sev

putLog' :: CliConfig -> Severity -> Text -> IO ()
putLog' conf sev t = runLoggingT (putLog sev t) (handleLog conf)

--TODO: Use optparse-applicative instead
-- Given the program's command line arguments, produce a reasonable CliConfig
mkDefaultCliConfig :: [String] -> IO CliConfig
mkDefaultCliConfig cliArgs = do
  let logLevel = if any (`elem` ["-v", "--verbose"]) cliArgs then Debug else Notice
  notInteractive <- not <$> isInteractiveTerm
  newCliConfig logLevel notInteractive notInteractive
  where
    isInteractiveTerm = do
      isTerm <- hIsTerminalDevice stdout
      -- Running in bash/fish/zsh completion
      let inShellCompletion = isInfixOf "completion" $ unwords cliArgs

      -- Respect the user’s TERM environment variable. Dumb terminals
      -- like Eshell cannot handle lots of control sequences that the
      -- spinner uses.
      termEnv <- lookupEnv "TERM"
      let isDumb = termEnv == Just "dumb"

      return $ isTerm && not inShellCompletion && not isDumb

newCliConfig
  :: Severity
  -> Bool
  -> Bool
  -> IO CliConfig
newCliConfig sev noColor noSpinner = do
  level <- newIORef sev
  lock <- newMVar False
  tipDisplayed <- newIORef False
  stack <- newIORef ([], [])
  textEncoding <- hGetEncoding stdout
  let theme = if maybe False supportsUnicode textEncoding
        then unicodeTheme
        else noUnicodeTheme
  return $ CliConfig level noColor noSpinner lock tipDisplayed stack theme

runCli :: MonadIO m => CliConfig -> CliT e m a -> m (Either e a)
runCli c =
    runExceptT
  . flip runLoggingT (handleLog c)
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

getLogLevel :: (MonadIO m, HasCliConfig m) => m Severity
getLogLevel = getLogLevel' =<< getCliConfig

getLogLevel' :: MonadIO m => CliConfig -> m Severity
getLogLevel' = liftIO . readIORef . _cliConfig_logLevel

setLogLevel :: (MonadIO m, HasCliConfig m) => Severity -> m ()
setLogLevel sev = do
  conf <- getCliConfig
  setLogLevel' conf sev

setLogLevel' :: MonadIO m => CliConfig -> Severity -> m ()
setLogLevel' conf sev = liftIO $ writeIORef (_cliConfig_logLevel conf) sev

handleLog :: MonadIO m => CliConfig -> Output -> m ()
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

-- | Intercept ExitFailure exceptions and log the given alert before exiting.
--
-- This is useful when you want to provide contextual information to a deeper failure.
withExitFailMessage :: (CliLog m, MonadCatch m) => Text -> m a -> m a
withExitFailMessage msg f = f `catch` \(e :: ExitCode) -> do
  case e of
    ExitFailure _ -> putLog Alert msg
    ExitSuccess -> pure ()
  throwM e

-- | Write log to stdout, with colors (unless `noColor`)
writeLog :: (MonadIO m, MonadMask m) => Bool -> Bool -> WithSeverity Text -> m ()
writeLog withNewLine noColor (WithSeverity severity s) = if T.null s then pure () else write
  where
    write
      | noColor && severity <= Warning = liftIO $ putFn $ T.pack (show severity) <> ": " <> s
      | not noColor && severity <= Error = TS.putStrWithSGR errorColors h withNewLine s
      | not noColor && severity <= Warning = TS.putStrWithSGR warningColors h withNewLine s
      | not noColor && severity >= Debug = TS.putStrWithSGR debugColors h withNewLine s
      | otherwise = liftIO $ putFn s

    putFn = if withNewLine then T.hPutStrLn h else T.hPutStr h
    h = if severity <= Error then stderr else stdout
    errorColors = [SetColor Foreground Vivid Red]
    warningColors = [SetColor Foreground Vivid Yellow]
    debugColors = [SetConsoleIntensity FaintIntensity]

-- | Allow the user to immediately switch to verbose logging upon pressing a particular key.
--
-- Call this function in a thread, and kill it to turn off keystroke monitoring.
allowUserToMakeLoggingVerbose
  :: CliConfig
  -> String  -- ^ The key to press in order to make logging verbose
  -> IO ()
allowUserToMakeLoggingVerbose conf keyCode = do
  let unlessVerbose f = do
        l <- getLogLevel' conf
        unless (l == verboseLogLevel) f
      showTip = liftIO $ forkIO $ unlessVerbose $ do
        liftIO $ threadDelay $ 10*1000000  -- Only show tip for actions taking too long (10 seconds or more)
        tipDisplayed <- liftIO $ atomicModifyIORef' (_cliConfig_tipDisplayed conf) $ (,) True
        unless tipDisplayed $ unlessVerbose $ do -- Check again in case the user had pressed Ctrl+e recently
          putLog' conf Notice "Tip: Press Ctrl+e to display full output"
  bracket showTip (liftIO . killThread) $ \_ -> do
    unlessVerbose $ do
      hSetBuffering stdin NoBuffering
      _ <- iterateUntil (== keyCode) getChars
      putLog' conf Warning "Ctrl+e pressed; making output verbose (-v)"
      setLogLevel' conf verboseLogLevel

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
