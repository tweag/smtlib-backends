{-# LANGUAGE OverloadedStrings #-}

module Examples (examples) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends (QueuingFlag (..), command, initSolver)
import qualified SMTLIB.Backends.Process as Process
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Process.Typed (getStdin)
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the 'Process' backend (running solvers as external
-- processes).
examples :: [TestTree]
examples =
  [ testCase "basic use" basicUse,
    testCase "setting options" setOptions,
    testCase "exiting manually" manualExit
  ]

-- | Basic use of the 'Process' backend.
basicUse :: IO ()
basicUse =
  -- 'Process.with' runs a computation using the 'Process' backend
  Process.with
    -- the default configuration uses Z3 as an external process and disables logging
    Process.defaultConfig
    $ \handle -> do
      -- first, we make the process handle into an actual backend
      let backend = Process.toBackend handle
      -- then, we create a solver out of the backend
      -- we enable queuing (it's faster !)
      solver <- initSolver Queuing backend
      -- we send a basic command to the solver and ignore the response
      -- we can write the command as a simple string because we have enabled the
      -- OverloadedStrings pragma
      _ <- command solver "(get-info :name)"
      -- note how there is no need to send an @(exit)@ command, this is already
      -- handled by the 'Process.with' function
      return ()

-- | An example of how to change the default settings of the 'Process' backend.
setOptions :: IO ()
setOptions =
  -- here we use a custom-made configuration
  let myConfig =
        Process.Config
          { Process.exe = "z3",
            Process.args = ["-in", "solver.timeout=10000"],
            Process.reportError = LBS.putStr . (`LBS.snoc` '\n')
          }
   in Process.with myConfig $ \handle -> do
        -- since the 'Process' module exposes its 'Handle' datatype entirely, we can also
        -- change the settings of the underlying process
        -- you probably won't need to do this as the library already choose these
        -- settings to ensure the communication with the solvers is as fast as
        -- possible
        let p = Process.process handle
            stdin = getStdin p
        -- for instance here we change the buffering mode of the process' input channel
        hSetBuffering stdin LineBuffering
        -- we can then use the backend as before
        let backend = Process.toBackend handle
        solver <- initSolver Queuing backend
        _ <- command solver "(get-info :name)"
        return ()

-- | An example of how to close the 'Process' backend's underlying process manually,
-- instead of relying on 'Process.with' or 'Process.close'.
manualExit :: IO ()
manualExit = do
  -- launch a new process with 'Process.new'
  handle <- Process.new Process.defaultConfig
  -- do some stuff
  doStuffWithHandle handle
  -- kill the process with 'Process.kill'
  -- other options include using 'Process.close' to ensure the process exits
  -- gracefully
  --
  -- if this isn't enough for you, it is always possible to send an @(exit)@
  -- command using 'Process.write', access the solver process using
  -- 'Process.process' and kill it manually
  -- if this is what you go with, don't forget to also cancel the
  -- 'Process.errorReader' asynchronous process!
  Process.kill handle
  where
    doStuffWithHandle _ = return ()
