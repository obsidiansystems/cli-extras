{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | An extension of `System.Process` that integrates with logging (`Obelisk.CLI.Logging`)
-- and is thus spinner friendly.
module Cli.Extras.Process
  ( AsProcessFailure (..)
  , ProcessFailure (..)
  , ProcessSpec (..)
  , callCommand
  , callProcess
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  , throwExitCode
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
  , runProc
  , runProcSilently
  , readProc
  ) where

import Control.Monad ((<=<), join, void)
import Control.Monad.Catch (MonadMask, bracketOnError)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (Prism', review)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Streams (InputStream, handleToInputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Concurrent (concurrentMerge)
import System.Process (CreateProcess, ProcessHandle, StdStream (CreatePipe), std_err, std_out)
import qualified System.Process as Process
import Text.ShellEscape (bash, bytes)
import qualified Data.Aeson as Aeson
import Control.Monad.Log (Severity (..))
import Cli.Extras.Logging (putLog, putLogRaw)
import Cli.Extras.Types (CliLog, CliThrow)

#if !(MIN_VERSION_base(4, 13, 0))
import Control.Monad.Fail (MonadFail)
#endif

data ProcessSpec = ProcessSpec
  { _processSpec_createProcess :: !CreateProcess
  , _processSpec_overrideEnv :: !(Maybe (Map String String -> Map String String))
  }

proc :: FilePath -> [String] -> ProcessSpec
proc cmd args = ProcessSpec (Process.proc cmd args) Nothing

shell :: String -> ProcessSpec
shell cmd = ProcessSpec (Process.shell cmd) Nothing

setEnvOverride :: (Map String String -> Map String String) -> ProcessSpec -> ProcessSpec
setEnvOverride f p = p { _processSpec_overrideEnv = Just f }

overCreateProcess :: (CreateProcess -> CreateProcess) -> ProcessSpec -> ProcessSpec
overCreateProcess f (ProcessSpec p x) = ProcessSpec (f p) x

setDelegateCtlc :: Bool -> ProcessSpec -> ProcessSpec
setDelegateCtlc b = overCreateProcess (\p -> p { Process.delegate_ctlc = b })

setCwd :: Maybe FilePath -> ProcessSpec -> ProcessSpec
setCwd fp = overCreateProcess (\p -> p { Process.cwd = fp })


-- TODO put back in `Obelisk.CliApp.Process` and use prisms for extensible exceptions
data ProcessFailure = ProcessFailure Process.CmdSpec Int -- exit code
  deriving Show

-- | Indicates arbitrary process failures form one variant (or conceptual projection) of
-- the error type.
class AsProcessFailure e where
  asProcessFailure :: Prism' e ProcessFailure

instance AsProcessFailure ProcessFailure where
  asProcessFailure = id

readProcessAndLogStderr
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadMask m)
  => Severity -> ProcessSpec -> m Text
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out

readProcessJSONAndLogStderr
  :: (Aeson.FromJSON a, MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadMask m)
  => Severity -> ProcessSpec -> m a
readProcessJSONAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  json <- liftIO $ BS.hGetContents out
  case Aeson.eitherDecodeStrict json of
    Right a -> pure a
    Left err -> do
      putLog Error $ "Could not decode process output as JSON: " <> T.pack err
      throwError $ review asProcessFailure $ ProcessFailure (Process.cmdspec $ _processSpec_createProcess process) 0

readCreateProcessWithExitCode
  :: (MonadIO m, CliLog m)
  => ProcessSpec -> m (ExitCode, String, String)
readCreateProcessWithExitCode procSpec = do
  process <- mkCreateProcess procSpec
  putLog Debug $ "Creating process: " <> reconstructProcSpec procSpec
  liftIO $ Process.readCreateProcessWithExitCode process ""

-- | Like 'System.Process.readProcess', but such that each of the child
-- processes' standard output streams (stdout and stderr) is logged,
-- with the corresponding severity.
--
-- Usually, this function is called as @readProcessAndLogOutput (Debug,
-- Error)@. If the child process is known to print diagnostic or
-- informative messages to stderr, it is advisable to call
-- 'readProcessAndLogOutput' with a non-Error severity for stderr, for
-- example @readProcessAndLogOutput (Debug, Debug)@.
readProcessAndLogOutput
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => (Severity, Severity)
  -- ^ This tuple controls the severity of each output stream. Its @fst@
  -- is the severity of stdout; @snd@ is the severity of stderr.
  -> ProcessSpec
  -> m Text
readProcessAndLogOutput (sev_out, sev_err) process = do
  (_, Just out, Just err, p) <- createProcess $ overCreateProcess
    (\p -> p { std_out = CreatePipe , std_err = CreatePipe }) process

  -- TODO interleave stdout and stderr in log correctly
  streamToLog =<< liftIO (streamHandle sev_err err)
  outText <- liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out
  putLogRaw sev_out outText

  outText <$ (throwExitCode process =<< waitForProcess p)

-- | Like 'System.Process.readProcess', but such that each of the child
-- processes' standard output streams (stdout and stderr) is logged,
-- with the corresponding severity.
--
-- Usually, this function is called as @callProcessAndLogOutput (Debug,
-- Error)@. If the child process is known to print diagnostic or
-- informative messages to stderr, it is advisable to call
-- 'callProcessAndLogOutput' with a non-Error severity for stderr, for
-- example @callProcessAndLogOutput (Debug, Debug)@.
callProcessAndLogOutput
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadMask m)
  => (Severity, Severity)
  -- ^ This tuple controls the severity of each output stream. Its @fst@
  -- is the severity of stdout; @snd@ is the severity of stderr.
  -> ProcessSpec
  -> m ()
callProcessAndLogOutput (sev_out, sev_err) process =
  void $ withProcess process $ \out err -> do
    stream <- liftIO $ join $ combineStream
      <$> streamHandle sev_out out
      <*> streamHandle sev_err err
    streamToLog stream
  where
    combineStream s1 s2 = concurrentMerge [s1, s2]

-- | Like 'System.Process.createProcess', but logging (with 'Debug'
-- severity) the process which was started.
createProcess
  :: (MonadIO m, CliLog m)
  => ProcessSpec -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess procSpec = do
  p <- mkCreateProcess procSpec
  putLog Debug $ "Creating process: " <> reconstructProcSpec procSpec
  liftIO $ Process.createProcess p

-- | Like 'System.Process.createProcess_', but logging (with 'Debug'
-- severity) the process which was started.
createProcess_
  :: (MonadIO m, CliLog m)
  => String -> ProcessSpec -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ name procSpec = do
  p <- mkCreateProcess procSpec
  putLog Debug $ "Creating process " <> T.pack name <> ": " <> reconstructProcSpec procSpec
  liftIO $ Process.createProcess_ name p

mkCreateProcess :: MonadIO m => ProcessSpec -> m Process.CreateProcess
mkCreateProcess (ProcessSpec p override') = case override' of
  Nothing -> pure p
  Just override -> do
    procEnv <- Map.fromList <$> maybe (liftIO getEnvironment) pure (Process.env p)
    pure $ p { Process.env = Just $ Map.toAscList (override procEnv) }

-- | Like 'System.Process.callProcess', but logging (with 'Debug'
-- severity) the process which was started.
callProcess
  :: (MonadIO m, CliLog m)
  => String -> [String] -> m ()
callProcess exe args = do
  putLog Debug $ "Calling process " <> T.pack exe <> " with args: " <> T.pack (show args)
  liftIO $ Process.callProcess exe args

-- | Like 'System.Process.callCommand', but logging (with 'Debug'
-- severity) the process which was started.
callCommand
  :: (MonadIO m, CliLog m)
  => String -> m ()
callCommand cmd = do
  putLog Debug $ "Calling command " <> T.pack cmd
  liftIO $ Process.callCommand cmd

withProcess
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadMask m)
  => ProcessSpec -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process f =
  bracketOnError
    (createProcess $ overCreateProcess
      (\x -> x { std_out = CreatePipe , std_err = CreatePipe }) process
    )
    (liftIO . Process.cleanupProcess)
    (\case
      (_, Just out, Just err, p) -> do
        f out err
        (out, err) <$ (throwExitCode process =<< waitForProcess p)
      _ -> error "withProcess: createProcess did not provide handles for CreatePipe as expected"
    )

-- | Runs a process to completion, aborting the computation (using
-- 'throwExitCode') in case of a non-'ExitSuccess' exit status.
runProcess_
  :: (MonadIO m, CliLog m, CliThrow e m, MonadMask m, AsProcessFailure e)
  => ProcessSpec -> m ()
runProcess_ process =
  bracketOnError
    (createProcess process)
    (liftIO . Process.cleanupProcess)
    (\(_, _, _, ph) -> throwExitCode process =<< waitForProcess ph)

-- Create an input stream from the file handle, associating each item with the given severity.
streamHandle :: Severity -> Handle -> IO (InputStream (Severity, BSC.ByteString))
streamHandle sev = Streams.map (sev,) <=< handleToInputStream

-- | Read from an input stream and log its contents
streamToLog
  :: (MonadIO m, CliLog m)
  => InputStream (Severity, BSC.ByteString) -> m ()
streamToLog stream = fix $ \loop -> do
  liftIO (Streams.read stream) >>= \case
    Nothing -> return ()
    Just (sev, line) -> putLogRaw sev (T.decodeUtf8With lenientDecode line) >> loop

-- | Wrapper around `System.Process.waitForProcess`
waitForProcess :: MonadIO m => ProcessHandle -> m ExitCode
waitForProcess = liftIO . Process.waitForProcess

-- | Aborts the computation (using 'throwError') when given a
-- non-'ExitSuccess' 'ExitCode'.
throwExitCode :: (CliThrow e m, AsProcessFailure e) => ProcessSpec -> ExitCode -> m ()
throwExitCode spec = \case
  ExitSuccess -> pure ()
  ExitFailure code -> throwError $ review asProcessFailure $ ProcessFailure (Process.cmdspec $ _processSpec_createProcess spec) code

-- | Pretty print a 'CmdSpec'
reconstructCommand :: Process.CmdSpec -> Text
reconstructCommand p = case p of
  Process.ShellCommand str -> T.pack str
  Process.RawCommand c as -> processToShellString c as
  where
    processToShellString cmd args = T.pack $ unwords $
      map (BSU.toString . bytes . bash . BSU.fromString) (cmd : args)

reconstructProcSpec :: ProcessSpec -> Text
reconstructProcSpec = reconstructCommand . Process.cmdspec . _processSpec_createProcess

-- | A wrapper for 'callProcessAndLogOutput' with sensible default
-- verbosities: standard output gets the 'Notice' severity and standard
-- error gets 'Error'.
runProc
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     ) => ProcessSpec -> m ()
runProc = callProcessAndLogOutput (Notice, Error)

-- | Like 'runProc', but the child process' output and error streams get
-- the 'Debug' severity.
runProcSilently
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     ) => ProcessSpec -> m ()
runProcSilently = callProcessAndLogOutput (Debug, Debug)

-- | A wrapper for 'readProcessAndLogOutput' with sensible default
-- verbosities: standard output gets the 'Debug' severity and standard
-- error gets 'Error'.
--
-- The child process' output gets the 'Debug' severity rather than the
-- 'Notice' severity because it is first and foremost /returned by this
-- function/, so you can log it afterwards in a reasonable manner.
readProc
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     ) => ProcessSpec -> m Text
readProc = readProcessAndLogOutput (Debug, Error)
