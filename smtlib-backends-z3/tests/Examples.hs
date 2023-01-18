{-# LANGUAGE OverloadedStrings #-}

module Examples (examples) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends (QueuingFlag (..), command, command_, flushQueue, initSolver)
import qualified SMTLIB.Backends.Z3 as Z3
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the 'Z3' backend (using Z3 as a library).
examples :: [TestTree]
examples =
  [ testCase "basic use" basicUse,
    testCase "setting options" settingOptions,
    testCase "flushing the queue" flushing
  ]

-- | Basic use of the 'Z3' backend.
basicUse :: IO ()
basicUse =
  -- 'Z3.with' runs a computation using the 'Z3' backend
  -- it takes a configuration object as argument, whose use we describe in
  -- 'settingOptions'
  -- here we just use the default configuration, literally @'Z3.Config' []@
  Z3.with Z3.defaultConfig $ \handle -> do
    -- first, we make the z3 handle into an actual backend
    let backend = Z3.toBackend handle
    -- then, we create a solver out of the backend
    -- we enable queuing (it's faster !)
    solver <- initSolver Queuing backend
    -- we send a basic command to the solver and ignore the response
    -- we can write the command as a simple string because we have enabled the
    -- OverloadedStrings pragma
    _ <- command solver "(get-info :name)"
    return ()

-- | How to set options at initialization time.
settingOptions :: IO ()
settingOptions =
  -- the Z3 C API is special (as a backend) in that some of its options can only be
  -- set when the object representing the state of the solver is created
  -- hence the 'Z3.new' and 'Z3.with' functions allow for setting options at
  -- initialization time
  Z3.with Z3.defaultConfig
  -- (Z3.Config [(":produce-unsat-cores", "true")])
  $
    \handle -> do
      -- we don't enable queuing so that commands are checked for correctness
      solver <- initSolver NoQueuing (Z3.toBackend handle)
      -- this is for example the case of the @:produce-assertions@ parameter, and not
      -- the case of the @:print-success@ one
      command_ solver "(set-option :print-success true)"
      -- the following would fail, returning
      -- @
      -- (error "line 1 column 33: error setting ':produce-unsat-cores',
      --         option value cannot be modified after initialization")
      -- @
      result <- command solver "(set-option :produce-unsat-cores true)"
      assertBool ("Expecting error message, got: " ++ LBS.unpack result) $ "(error" `LBS.isPrefixOf` result
      return ()

-- | An example on how to force the content of the queue to be evaluated.
flushing :: IO ()
flushing = do
  -- sometimes you want to use 'Queuing' mode but still force some commands not
  -- producing any output to be evaluated
  -- in that case, using 'command' would lead to your program hanging as it waits
  -- for a response from the solver that never comes
  -- the solution is to use the 'command_' function and then to flush the queue
  Z3.with Z3.defaultConfig $ \handle -> do
    -- this example only makes sense in queuing mode
    solver <- initSolver Queuing $ Z3.toBackend handle
    -- add a command to the queue
    command_ solver "(assert true)"
    -- force the queue to be evaluated
    flushQueue solver
