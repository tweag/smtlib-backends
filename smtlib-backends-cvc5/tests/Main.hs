{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import EdgeCases (edgeCases)
import Examples (examples)
import SMTLIB.Backends
import qualified SMTLIB.Backends.CVC5 as CVC5
import SMTLIB.Backends.Tests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testBackend "Basic examples" validSources cvc5,
        testGroup "API usage examples" examples,
        testBackend "Error handling" failingSources cvc5,
        testGroup "Edge cases" edgeCases
      ]
  where
    cvc5 todo = CVC5.with CVC5.defaultConfig $ todo . CVC5.toBackend
    validSources = filter (\source -> name source `notElem` ["assertions", "unsat cores"]) sources
    failingSources =
      [ Source "invalid command" $ \solver -> do
          result <- command solver "this is not a valid command!!!"
          assertBool ("Expecting error message, got: " ++ LBS.unpack result) $ "(error" `LBS.isPrefixOf` result
      ]
