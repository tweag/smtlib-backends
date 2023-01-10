{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import Examples (examples)
import SMTLIB.Backends
import SMTLIB.Backends.Tests
import qualified SMTLIB.Backends.Z3 as Z3
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testBackend "Basic examples" validSources z3,
        testGroup "API usage examples" examples,
        testBackend "Error handling" failingSources z3
      ]
  where
    z3 todo = Z3.with Z3.defaultConfig $ todo . Z3.toBackend
    validSources = filter (\source -> name source `notElem` ["assertions", "unsat cores"]) sources
    failingSources =
      [ Source "invalid command" $ \solver -> do
          result <- command solver "this is not a valid command!!!"
          assertBool ("Expecting error message, got: " ++ LBS.unpack result) $ "(error" `LBS.isPrefixOf` result
      ]
