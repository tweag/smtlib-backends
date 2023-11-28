{-# LANGUAGE OverloadedStrings #-}

module EdgeCases (edgeCases) where

import Data.ByteString.Builder (Builder)
import SMTLIB.Backends as SMT
import qualified SMTLIB.Backends.CVC5 as CVC5
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
checkEmptyResponse cmd = CVC5.with CVC5.defaultConfig $ \handle -> do
  let backend = CVC5.toBackend handle
  response <- SMT.send backend cmd
  assertEqual "expected no response" "" response
