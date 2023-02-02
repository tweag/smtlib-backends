{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | A module providing a backend that launches solvers as external processes.
module SMTLIB.Backends.Process
  ( Config (..),
    Handle (..),
    defaultConfig,
    new,
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
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (Default, def)
import GHC.IO.Exception (IOException (ioe_description))
import SMTLIB.Backends (Backend (..))
import qualified System.IO as IO
import System.Process as P

data Config = Config
  { -- | The command to call to run the solver.
    exe :: String,
    -- | Arguments to pass to the solver's command.
    args :: [String],
    -- | A function for logging the solver process' messages on stderr and file
    -- handle exceptions.
    -- If you want line breaks between each log message, you need to implement
    -- it yourself, e.g use @'LBS.putStr' . (<> "\n")@.
    reportError :: LBS.ByteString -> IO ()
  }

-- | By default, use Z3 as an external process and ignores log messages.
defaultConfig :: Config
-- if you change this, make sure to also update the comment two lines above
-- as well as the one in @smtlib-backends-process/tests/Examples.hs@
defaultConfig = Config "z3" ["-in"] (\_ -> return ())

instance Default Config where
  def = defaultConfig

data Handle = Handle
  { -- | The process running the solver.
    process :: P.ProcessHandle,
    -- | The input channel of the process.
    hIn :: IO.Handle,
    -- | The output channel of the process.
    hOut :: IO.Handle,
    -- | The error channel of the process.
    hErr :: IO.Handle,
    -- | A process reading the solver's error messages and logging them.
    errorReader :: Async ()
  }

-- | Run a solver as a process.
new ::
  -- | The solver process' configuration.
  Config ->
  IO Handle
new Config {..} = decorateIOError "creating the solver process" $ do
  (Just hIn, Just hOut, Just hErr, process) <-
    P.createProcess
      (P.proc exe args)
        { std_in = P.CreatePipe,
          std_out = P.CreatePipe,
          std_err = P.CreatePipe
        }
  mapM_ setupHandle [hIn, hOut, hErr]
  -- log error messages created by the backend
  errorReader <-
    async $
      forever
        ( do
            errs <- BS.hGetLine hErr
            reportError' errs
        )
        `X.catch` \X.SomeException {} ->
          return ()
  return $ Handle process hIn hOut hErr errorReader
  where
    setupHandle h = do
      IO.hSetBinaryMode h True
      IO.hSetBuffering h $ IO.BlockBuffering Nothing
    reportError' = reportError . LBS.fromStrict

-- | Send a command to the process without reading its response.
write :: Handle -> Builder -> IO ()
write Handle {..} cmd =
  decorateIOError msg $ do
    hPutBuilder hIn $ cmd <> "\n"
    IO.hFlush hIn
  where
    msg = "sending command " ++ show (toLazyByteString cmd) ++ " to the solver"

-- | Cleanup the process' resources, terminate it and wait for it to actually exit.
close :: Handle -> IO ()
close Handle {..} = decorateIOError "closing the solver process" $ do
  cancel errorReader
  P.cleanupProcess (Just hIn, Just hOut, Just hErr, process)

-- | Create a solver process, use it to make a computation and close it.
with ::
  -- | The solver process' configuration.
  Config ->
  -- | The computation to run with the solver process
  (Handle -> IO a) ->
  IO a
with config = X.bracket (new config) close

infixr 5 :<

pattern (:<) :: Char -> BS.ByteString -> BS.ByteString
pattern c :< rest <- (BS.uncons -> Just (c, rest))

-- | Make the solver process into an SMT-LIB backend.
toBackend :: Handle -> Backend
toBackend handle = Backend backendSend backendSend_
  where
    backendSend_ = write handle
    backendSend cmd = do
      -- exceptions are decorated inside the body of 'write'
      write handle cmd
      decorateIOError "reading solver's response" $
        toLazyByteString
          <$> continueNextLine (scanParen 0) mempty

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
      next <-
        BS.hGetLine (hOut handle) `X.catch` \ex ->
          X.throwIO
            ( ex
                { ioe_description =
                    ioe_description ex
                      ++ ": "
                      ++ show (toLazyByteString acc)
                }
            )
      f (acc <> byteString next) next

decorateIOError :: String -> IO a -> IO a
decorateIOError contextDescription =
  X.handle $ \ex ->
    X.throwIO
      ( ex
          { ioe_description =
              "[smtlib-backends-process] while "
                ++ contextDescription
                ++ ": "
                ++ ioe_description ex
          }
      )
