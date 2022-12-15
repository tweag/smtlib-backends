{-# LANGUAGE OverloadedStrings #-}

module Examples (solverTests, processTests) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (def)
import SMTLIB.Backends (Backend, command, command_, initSolver)
import qualified SMTLIB.Backends.Process as Process
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Process.Typed (getStdin)
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the 'Backends' module.
solverTests :: TestTree
solverTests =
  testGroup
    "API use examples"
    [ testCase "basic use" solverBasicUse
    ]

-- | Basic use of the 'Solver' datatype.
solverBasicUse :: IO ()
solverBasicUse =
  -- we need a solver backend to evaluate our commands
  -- examples on how to create such backends can be found in the corresponding
  -- sections, e.g. 'processBasicUse'
  withBackend $ \backend -> do
    -- then, we create a solver out of the backend
    -- we enable queuing (it's faster !)
    solver <- initSolver backend True
    -- we send a basic command to the solver and ignore the response
    -- we can write the command as a simple string because we have enabled the
    -- OverloadedStrings pragma
    _ <- command solver "(get-info :name)"
    return ()
  where
    withBackend :: (Backend -> IO a) -> IO a
    withBackend = Process.with def . (. Process.toBackend)

-- | The examples for the 'Process' backend (running solvers as external
-- processes).
processTests :: TestTree
processTests =
  testGroup
    "API use examples"
    [ testCase "basic use" processBasicUse,
      testCase "setting options" processSetOptions,
      testCase "exiting manually" processManualExit
    ]

-- | Basic use of the 'Process' backend.
processBasicUse :: IO ()
processBasicUse =
  -- 'Process.with' runs a computation using the 'Process' backend
  Process.with
    -- the configuration type 'Process.Config' is an instance of the 'Default' class
    -- we can thus use a default configuration for the backend with the 'def' method
    -- this default configuration uses Z3 as an external process and disables logging
    def
    $ \handle -> do
      -- first, we make the process handle into an actual backend
      let backend = Process.toBackend handle
      -- then, we create a solver out of the backend
      -- we enable queuing (it's faster !)
      solver <- initSolver backend True
      -- we send a basic command to the solver and ignore the response
      -- we can write the command as a simple string because we have enabled the
      -- OverloadedStrings pragma
      _ <- command solver "(get-info :name)"
      return ()

-- | An example of how to set options for the 'Process' backend.
processSetOptions :: IO ()
processSetOptions =
  -- here we use a custom-made configuration
  let myConfig =
        Process.Config
          { Process.exe = "z3",
            Process.args = ["-in", "solver.timeout=10000"],
            Process.reportError = LBS.putStr . ('LBS.snoc' '\n')
          }
   in Process.with myConfig $ \handle -> do
        -- since the 'Process' module exposes its 'Handle' datatype entirely, we can also
        -- set the options of the underlying process
        let p = Process.process handle
            stdin = getStdin p
        -- for instance here we change the buffering mode of the process' input channel
        hSetBuffering stdin LineBuffering
        -- we can then use the backend as before
        let backend = Process.toBackend handle
        solver <- initSolver backend True
        _ <- command solver "(get-info :name)"
        return ()

-- | An example of how to close the 'Process' backend's underlying process manually,
-- instead of relying on 'Process.with' or 'Process.close'.
processManualExit :: IO ()
processManualExit = do
  -- launch a new process with 'Process.new'
  handle <- Process.new def
  let backend = Process.toBackend handle
  -- here we disable queuing so that we can use 'command_' to ensure the exit
  -- command will be received successfully
  solver <- initSolver backend False
  command_ solver "(exit)"
  -- 'Process.wait' takes care of cleaning resources and waits for the process to
  -- exit
  exitCode <- Process.wait handle
  assertBool "the solver process didn't exit properly" $ exitCode == ExitSuccess
