-- | A module providing functions useful for testing a backend for SimpleSMT.
module SMTLIB.Backends.Tests
  ( testBackend,
    Src.Source (..),
    Src.sources,
  )
where

import Data.ByteString.Builder (Builder)
import SMTLIB.Backends (Backend, initSolver)
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
  (Builder -> IO ()) ->
  -- | A function that should create a backend, run a given
  -- computation and release the backend's resources.
  ((Backend -> Assertion) -> Assertion) ->
  TestTree
testBackend name sources logger with =
  testGroup name $ do
    lazyMode <- [False, True]
    return
      $ testGroup
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
