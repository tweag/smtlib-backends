{-# LANGUAGE OverloadedStrings #-}

module EdgeCases (edgeCases) where

import Control.Monad (when)
import Data.ByteString.Builder (Builder)
import SMTLIB.Backends as SMT
import qualified SMTLIB.Backends.Process as Process
import System.IO (hGetContents', hReady)
import System.Process.Typed (getStdout)
import Test.Tasty
import Test.Tasty.HUnit

edgeCases :: [TestTree]
edgeCases =
  [ testCase "Piling up stopping procedures" pileUpStops,
    testCase "Sending an empty command" emptyCommand,
    testCase "Sending a command expecting no response" commandNoResponse
  ]

-- | It's possible to accumulate procedures that stop the backend without
-- hanging or crashing the program.
pileUpStops :: IO ()
pileUpStops = Process.with Process.defaultConfig $ \handle -> do
  let backend = Process.toBackend handle
  SMT.send_ backend "(exit)"
  _ <- Process.close handle
  Process.kill handle

-- | Upon processing an empty command, the backend will not respond.
emptyCommand :: IO ()
emptyCommand = checkNoResponse ""

-- | Upon processing a command producing no output, the backend will not
-- respond, not even with an empty line.
commandNoResponse :: IO ()
commandNoResponse = checkNoResponse "(set-option :print-success false)"

checkNoResponse :: Builder -> IO ()
checkNoResponse cmd = Process.with Process.defaultConfig $ \handle -> do
  let backend = Process.toBackend handle
      stdout = getStdout $ Process.process handle
  SMT.send_ backend cmd
  responseAvailable <- hReady stdout
  when responseAvailable $ do
    response <- hGetContents' stdout
    assertFailure $ "expected no response, got: '" <> response <> "'"
