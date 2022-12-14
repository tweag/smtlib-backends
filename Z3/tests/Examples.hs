{-# LANGUAGE OverloadedStrings #-}

module Examples (z3Tests) where

import SMTLIB.Backends (command, initSolver)
import qualified SMTLIB.Backends.Z3 as Z3
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the `Process` backend.
z3Tests :: TestTree
z3Tests =
  testGroup
    "API use examples"
    [testCase "basic use" z3BasicUse]

-- | Basic use of the `Z3` backend.
z3BasicUse :: IO ()
z3BasicUse =
  -- `Z3.with` runs a computation using the `Z3` backend
  Z3.with $ \handle -> do
    -- first, we make the z3 handle into an actual backend
    let backend = Z3.toBackend handle
    -- then, we create a solver out of the backend
    -- we enable queuing (it's faster !)
    solver <- initSolver backend True
    -- we send a basic command to the solver and ignore the response
    -- we can write the command as a simple string because we have enabled the
    -- OverloadedStrings pragma
    _ <- command solver "(get-info :name)"
    return ()
