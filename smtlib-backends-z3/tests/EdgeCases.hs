{-# LANGUAGE OverloadedStrings #-}

module EdgeCases (edgeCases) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends as SMT
import qualified SMTLIB.Backends.Z3 as Z3
import Test.Tasty
import Test.Tasty.HUnit

edgeCases :: [TestTree]
edgeCases =
  [ testCase "Sending an empty command" emptyCommand,
    testCase "Sending a command expecting no response" commandNoResponse
  ]

-- | Upon processing an empty command, the backend will respond with an empty output.
emptyCommand :: IO ()
emptyCommand = checkEmptyResponse ""

-- | Upon processing a command producing no output, the backend will respond
-- with an empty output.
commandNoResponse :: IO ()
commandNoResponse = checkEmptyResponse "(set-option :print-success false)"

checkEmptyResponse :: Builder -> IO ()
checkEmptyResponse cmd = Z3.with Z3.defaultConfig $ \handle -> do
  let backend = Z3.toBackend handle
  response <- SMT.send backend cmd
  assertEqual "expected no response" "" response
