{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | A module providing a backend that launches solvers as external processes.
module SMTLIB.Backends.Process
  ( SolverProcess (..),
    new,
    wait,
    stop,
    with,
    toBackend,
  )
where

import SMTLIB.Backends (Backend(..))

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
import System.Exit (ExitCode)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
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

data SolverProcess = SolverProcess
  { -- | The process running the solver.
    process :: Process Handle Handle Handle,
    -- | A process reading the solver's error messages and logging them.
    errorReader :: Async ()
  }

-- | Run a solver as a process.
new ::
  -- | The command to run the solver.
  String ->
  -- | Arguments to pass to the solver's command.
  [String] ->
  -- | A function for logging the solver's creation, errors and termination.
  (BS.ByteString -> IO ()) ->
  IO SolverProcess
new exe args logger = do
  solverProcess <-
    startProcess $
      setStdin createLoggedPipe $
        setStdout createLoggedPipe $ setStderr createLoggedPipe $ P.proc exe args
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
  return $ SolverProcess solverProcess solverErrorReader
  where
    createLoggedPipe =
      mkPipeStreamSpec $ \_ h -> do
        hSetBinaryMode h True
        hSetBuffering h $ BlockBuffering Nothing
        return
          ( h,
            hClose h `X.catch` \ex ->
              logger $ BS.pack $ show (ex :: X.IOException)
          )

-- | Wait for the process to exit and cleanup its resources.
wait :: SolverProcess -> IO ExitCode
wait solver = do
  cancel $ errorReader solver
  waitExitCode $ process solver

-- | Terminate the process, wait for it to actually exit and cleanup its resources.
stop :: SolverProcess -> IO ()
stop solver = do
  cancel $ errorReader solver
  stopProcess $ process solver

-- | Create a solver process, use it to make a computation and stop it.
with :: String -> [String] -> (BS.ByteString -> IO ()) -> (SolverProcess -> IO a) -> IO a
with exe args logger = X.bracket (new exe args logger) stop

infixr 5 :<

pattern (:<) :: Char -> BS.ByteString -> BS.ByteString
pattern c :< rest <- (BS.uncons -> Just (c, rest))

-- | Make the solver process into an SMT-LIB backend.
toBackend :: SolverProcess -> Backend
toBackend solver =
  Backend $ \cmd -> do
    hPutBuilder (getStdin $ process solver) $ cmd <> "\n"
    hFlush $ getStdin $ process solver
    toLazyByteString <$> continueNextLine (scanParen 0) mempty
  where
    -- scanParen read lines from the solver's output channel until it has detected
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
      next <- BS.hGetLine $ getStdout $ process solver
      f (acc <> byteString next) next
