{-# LANGUAGE OverloadedStrings #-}

module Examples (examples) where

import SMTLIB.Backends (QueuingFlag (..), command, command_, flushQueue, initSolver)
import qualified SMTLIB.Backends.CVC5 as CVC5
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the 'CVC5' backend (using CVC5 as a library).
examples :: [TestTree]
examples =
  [ testCase "basic use" basicUse,
    testCase "flushing the queue" flushing
  ]

-- | Basic use of the 'Z3' backend.
basicUse :: IO ()
basicUse =
  -- 'CVC5.with' runs a computation using the 'CVC5' backend
  -- it takes a configuration object as argument, whose use we describe in
  -- 'settingOptions'
  -- here we just use the default configuration, literally @'CVC5.Config' []@
  CVC5.with CVC5.defaultConfig $ \handle -> do
    -- first, we make the CVC5 handle into an actual backend
    let backend = CVC5.toBackend handle
    -- then, we create a solver out of the backend
    -- we enable queuing (it's faster !)
    solver <- initSolver Queuing backend
    -- we send a basic command to the solver and ignore the response
    -- we can write the command as a simple string because we have enabled the
    -- OverloadedStrings pragma
    _ <- command solver "(get-info :name)"
    return ()

-- | An example on how to force the content of the queue to be evaluated.
flushing :: IO ()
flushing = do
  -- sometimes you want to use 'Queuing' mode but still force some commands not
  -- producing any output to be evaluated
  -- in that case, using 'command' would lead to your program hanging as it waits
  -- for a response from the solver that never comes
  -- the solution is to use the 'command_' function and then to flush the queue
  CVC5.with CVC5.defaultConfig $ \handle -> do
    -- this example only makes sense in queuing mode
    solver <- initSolver Queuing $ CVC5.toBackend handle
    -- add a command to the queue
    command_ solver "(assert true)"
    -- force the queue to be evaluated
    flushQueue solver
