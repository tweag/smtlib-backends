{-# LANGUAGE OverloadedStrings #-}

module SMTLIB.Backends
  ( Backend (..),
    QueuingFlag (..),
    Solver,
    initSolver,
    command,
    command_,
    flushQueue,
  )
where

import Control.Monad ((<=<))
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isSpace)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Prelude hiding (log)

-- | The type of solver backends. SMTLib2 commands are sent to a backend which
-- processes them and outputs the solver's response.
data Backend = Backend
  { -- | Send a command to the backend.
    -- While the implementation depends on the backend, this function is usually
    -- *not* thread-safe.
    send :: Builder -> IO LBS.ByteString,
    -- | Send a command that doesn't produce any response to the backend.
    -- The backend may implement this by not reading the output and leaving it
    -- for a later read, or reading the output and discarding it immediately.
    -- Hence this method should only be used when the command does not produce
    -- any response to be outputted.
    -- Again, this function may not be thread-safe.
    send_ :: Builder -> IO ()
  }

type Queue = IORef Builder

-- | A boolean-equivalent datatype indicating whether to enable queuing.
data QueuingFlag = Queuing | NoQueuing

-- | Push a command on the solver's queue of commands to evaluate.
-- The command must not produce any output when evaluated, unless it is the last
-- command added before the queue is flushed.
-- For a fixed queue, this function is *not* thread-safe.
put :: Queue -> Builder -> IO ()
put q cmd = modifyIORef q (<> cmd)

-- | Empty the queue of commands to evaluate and return its content as a bytestring
-- builder.
-- For a fixed queue, this function is *not* thread-safe.
flush :: Queue -> IO Builder
flush q = do
  cmds <- readIORef q
  writeIORef q mempty
  return cmds

-- | A solver is essentially a wrapper around a solver backend. It also comes an
-- optional queue of commands to send to the backend.
--
-- A solver can either be in 'Queuing' mode or 'NoQueuing' mode. In 'NoQueuing'
-- mode, the queue of commands isn't used and the commands are sent to the
-- backend immediately. In 'Queuing' mode, commands whose output are not
-- strictly necessary for the rest of the computation (typically the ones whose
-- output should just be @success@) and that are sent through 'command_' are not
-- sent to the backend immediately, but rather written on the solver's queue.
-- When a command whose output is actually necessary needs to be sent, the queue
-- is flushed and sent as a batch to the backend.
--
-- 'Queuing' mode should be faster as there usually is a non-negligible constant
-- overhead in sending a command to the backend. But since the commands are sent
-- by batches, a command sent to the solver will only produce an error when the
-- queue is flushed, i.e. when a command with interesting output is sent. You
-- thus probably want to stick with 'NoQueuing' mode when debugging. Moreover,
-- when commands are sent by batches, only the last command in the batch may
-- produce an output for parsing to work properly. Hence the @:print-success@
-- option is disabled in 'Queuing' mode, and this should not be overriden
-- manually.
data Solver = Solver
  { -- | The backend processing the commands.
    backend :: Backend,
    -- | An optional queue to write commands that are to be sent to the solver lazily.
    queue :: Maybe Queue
  }

-- | Create a new solver and initialize it with some options so that it behaves
-- correctly for our use.
-- In particular, the "print-success" option is disabled in 'Queuing' mode. This
-- should not be overriden manually.
initSolver ::
  -- | whether to enable 'Queuing' mode (see 'Solver' for the meaning of this
  -- flag)
  QueuingFlag ->
  -- | the solver backend
  Backend ->
  IO Solver
initSolver queuing solverBackend = do
  solverQueue <- case queuing of
    Queuing -> do
      ref <- newIORef mempty
      return $ Just ref
    NoQueuing -> return Nothing
  let solver = Solver solverBackend solverQueue
  case queuing of
    Queuing -> return ()
    NoQueuing ->
      -- this should not be enabled when the queue is used, as it messes with parsing
      -- the outputs of commands that are actually interesting
      --
      -- TODO checking for correctness and enabling laziness can be made compatible
      -- but it would require the solver backends to return several outputs at once
      -- alternatively, we may consider that the user wanting both features should
      -- implement their own backend that deals with this
      command_ solver "(set-option :print-success true)"
  return solver

-- | Have the solver evaluate a SMT-LIB command.
-- This forces the queued commands to be evaluated as well, but their results are
-- *not* checked for correctness.
--
-- Concurrent calls to different solvers are thread-safe, but not concurrent
-- calls on the same solver or the same backend.
--
-- Only one command must be given per invocation, or the multiple commands must
-- together produce the output of one command only.
command :: Solver -> Builder -> IO LBS.ByteString
command solver cmd = do
  send (backend solver)
    =<< case queue solver of
      Nothing -> return cmd
      Just q -> (<> cmd) <$> flush q

-- | A command with no interesting result.
-- In 'NoQueuing' mode, the result is checked for correctness. In 'Queuing'
-- mode, (unless the queue is flushed and evaluated right after) the command
-- must not produce any output when evaluated, and its output is thus in
-- particular not checked for correctness.
--
-- Concurrent calls to different solvers are thread-safe, but not concurrent
-- calls on the same solver or the same backend.
--
-- Only one command must be given per invocation, or the multiple commands must
-- together produce the output of one command only.
command_ :: Solver -> Builder -> IO ()
command_ solver cmd =
  case queue solver of
    Nothing -> do
      res <- send (backend solver) cmd
      if trim res == "success"
        then return ()
        else
          fail $
            unlines
              [ "Unexpected result from the SMT solver:",
                "  Expected: success",
                "  Got: " ++ show res
              ]
    Just q -> put q cmd
  where
    trim = LBS.dropWhile isSpace . LBS.reverse . LBS.dropWhile isSpace . LBS.reverse

-- | Force the content of the queue to be sent to the solver.
-- Only useful in queuing mode, does nothing in non-queuing mode.
flushQueue :: Solver -> IO ()
flushQueue solver = maybe (return ()) (send_ (backend solver) <=< flush) $ queue solver
