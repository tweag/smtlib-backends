{-# LANGUAGE OverloadedStrings #-}

-- | A module providing functions useful for testing a backend for SimpleSMT.
module SMTLIB.Backends.Tests
  ( testBackend,
    Src.Source (..),
    Src.sources,
  )
where

import Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends (Backend, LogType, command, initSolver)
import qualified SMTLIB.Backends.Tests.Sources as Src
import Test.Tasty
import Test.Tasty.HUnit

-- | Test a backend by using it to run a list of examples.
testBackend ::
  -- | The name of the test group.
  String ->
  -- | A list of examples on which to run the backend.
  [Src.Source] ->
  -- | A function for logging the solver's activity.
  (LogType -> LBS.ByteString -> IO ()) ->
  -- | A function that should create a backend, run a given
  -- computation and release the backend's resources.
  ((Backend -> Assertion) -> Assertion) ->
  TestTree
testBackend name sources logger with =
  testGroup name $ do
    lazyMode <- [False, True]
    return $
      testGroup
        ( if lazyMode
            then "lazy"
            else "eager"
        )
        $ do
          source <- sources
          return $
            testCase (Src.name source) $
              with $ \backend -> do
                solver <- initSolver backend lazyMode logger
                Src.run source solver
                -- ensure the sources consisting only of ackCommands also run
                _ <- command solver "(get-info :name)"
                return ()
