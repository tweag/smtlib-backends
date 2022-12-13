{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends
import SMTLIB.Backends.Tests
import qualified SMTLIB.Backends.Z3 as Z3
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain $
    testGroup "Tests" $
      [ testBackend "Examples" validSources noLogging z3,
        testBackend "Error handling" failingSources noLogging z3
      ]
  where
    z3 todo = Z3.with $ todo . Z3.toBackend
    noLogging = \_ _ -> return ()
    validSources = filter (\source -> name source `notElem` ["assertions", "unsat cores"]) sources
    failingSources =
      [ Source "invalid command" $ \solver -> do
          result <- command solver "this is not a valid command!!!"
          assertBool ("Expecting error message, got: " ++ LBS.unpack result) $ "(error" `LBS.isPrefixOf` result
      ]
