{-# LANGUAGE OverloadedStrings #-}

import Examples (examples)
import qualified SMTLIB.Backends.Process as Process
import SMTLIB.Backends.Tests (sources, testBackend)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testBackend "Basic examples" sources $ \todo ->
          Process.with Process.defaultConfig $ todo . Process.toBackend,
        testGroup "API usage examples" examples,
        testCase "Piling up stopping procedures" $
          Process.with Process.defaultConfig $ \handle -> do
            Process.write handle "(exit)"
            _ <- Process.close handle
            Process.kill handle
      ]
