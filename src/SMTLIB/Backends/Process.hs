{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | A module providing a backend that launches solvers as external processes.
module SMTLIB.Backends.Process
  ( Logger (Logger),
    Config (..),
    Handle,
    new,
    wait,
    close,
    with,
    toBackend,
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import qualified Control.Exception as X
import Control.Monad (forever)
import Data.ByteString.Builder
  ( Builder,
    byteString,
    hPutBuilder,
    toLazyByteString,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Default (Default, def)
import SMTLIB.Backends (Backend (..))
import System.Exit (ExitCode)
import qualified System.IO as IO
import System.Process.Typed
  ( Process,
    getStderr,
    getStdin,
    getStdout,
    mkPipeStreamSpec,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    waitExitCode,
  )
import qualified System.Process.Typed as P (proc)

-- * Some logging facilities

-- | A simple wrapper for loggers.
newtype Logger = Logger (BS.ByteString -> IO ())

-- | Log nothing by default.
instance Default Logger where
  def = Logger $ const $ return ()

-- * Configuration of the solver

data Config = Config
  { -- | The command to call to run the solver.
    exe :: String,
    -- | Arguments to pass to the solver's command.
    args :: [String]
  }

-- * Interaction with solver processes

data Handle = Handle
  { -- | The process running the solver.
    process :: Process IO.Handle IO.Handle IO.Handle,
    -- | A process reading the solver's error messages and logging them.
    errorReader :: Async ()
  }

-- | Run a solver as a process.
new ::
  -- | The solver process' configuration.
  Config ->
  -- | A function for logging the solver's creation, errors and termination.
  Logger ->
  IO Handle
new config (Logger logger) = do
  solverProcess <-
    startProcess $
      setStdin createLoggedPipe $
        setStdout createLoggedPipe $
          setStderr createLoggedPipe $
            P.proc (exe config) (args config)
  -- log error messages created by the backend
  solverErrorReader <-
    async $
      forever
        ( do
            errs <- BS.hGetLine $ getStderr solverProcess
            logger $ "[stderr] " <> errs
        )
        `X.catch` \X.SomeException {} ->
          return ()
  return $ Handle solverProcess solverErrorReader
  where
    createLoggedPipe =
      mkPipeStreamSpec $ \_ h -> do
        IO.hSetBinaryMode h True
        IO.hSetBuffering h $ IO.BlockBuffering Nothing
        return
          ( h,
            IO.hClose h `X.catch` \ex ->
              logger $ BS.pack $ show (ex :: X.IOException)
          )

-- | Wait for the process to exit and cleanup its resources.
wait :: Handle -> IO ExitCode
wait handle = do
  cancel $ errorReader handle
  waitExitCode $ process handle

-- | Terminate the process, wait for it to actually exit and cleanup its resources.
close :: Handle -> IO ()
close handle = do
  cancel $ errorReader handle
  stopProcess $ process handle

-- | Create a solver process, use it to make a computation and stop it.
with :: Config -> Logger -> (Handle -> IO a) -> IO a
with config logger = X.bracket (new config logger) close

infixr 5 :<

pattern (:<) :: Char -> BS.ByteString -> BS.ByteString
pattern c :< rest <- (BS.uncons -> Just (c, rest))

-- | Make the solver process into an SMT-LIB backend.
toBackend :: Handle -> Backend
toBackend handle =
  Backend $ \cmd -> do
    hPutBuilder (getStdin $ process handle) $ cmd <> "\n"
    IO.hFlush $ getStdin $ process handle
    toLazyByteString <$> continueNextLine (scanParen 0) mempty
  where
    -- scanParen read lines from the handle's output channel until it has detected
    -- a complete s-expression, i.e. a well-parenthesized word that may contain
    -- strings, quoted symbols, and comments
    -- if we detect a ')' at depth 0 that is not enclosed in a string, a quoted
    -- symbol or a comment, we give up and return immediately
    -- see also the SMT-LIB standard v2.6
    -- https://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2021-05-12.pdf#part.2
    scanParen :: Int -> Builder -> BS.ByteString -> IO Builder
    scanParen depth acc ('(' :< more) = scanParen (depth + 1) acc more
    scanParen depth acc ('"' :< more) = do
      (acc', more') <- string acc more
      scanParen depth acc' more'
    scanParen depth acc ('|' :< more) = do
      (acc', more') <- quotedSymbol acc more
      scanParen depth acc' more'
    scanParen depth acc (';' :< _) = continueNextLine (scanParen depth) acc
    scanParen depth acc (')' :< more)
      | depth <= 1 = return acc
      | otherwise = scanParen (depth - 1) acc more
    scanParen depth acc (_ :< more) = scanParen depth acc more
    -- mempty case
    scanParen 0 acc _ = return acc
    scanParen depth acc _ = continueNextLine (scanParen depth) acc

    string :: Builder -> BS.ByteString -> IO (Builder, BS.ByteString)
    string acc ('"' :< '"' :< more) = string acc more
    string acc ('"' :< more) = return (acc, more)
    string acc (_ :< more) = string acc more
    -- mempty case
    string acc _ = continueNextLine string acc

    quotedSymbol :: Builder -> BS.ByteString -> IO (Builder, BS.ByteString)
    quotedSymbol acc ('|' :< more) = return (acc, more)
    quotedSymbol acc (_ :< more) = string acc more
    -- mempty case
    quotedSymbol acc _ = continueNextLine quotedSymbol acc

    continueNextLine :: (Builder -> BS.ByteString -> IO a) -> Builder -> IO a
    continueNextLine f acc = do
      next <- BS.hGetLine $ getStdout $ process handle
      f (acc <> byteString next) next
