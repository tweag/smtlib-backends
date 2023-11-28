{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module providing a backend that sends commands to CVC5 using its C API.
module SMTLIB.Backends.CVC5
  ( Config (..),
    Handle,
    defaultConfig,
    new,
    close,
    with,
    toBackend,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra
  ( defaultChunkSize,
    smallChunkSize,
    toLazyByteStringWith,
    untrimmedStrategy,
  )
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Unsafe
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import SMTLIB.Backends (Backend (..))

data CVC5Solver

data CVC5Parser

newtype Config = Config
  { -- | A list of options to set during the solver's initialization.
    -- Each pair is of the form @(paramId, paramValue)@, e.g.
    -- @(":produce-models", "true")@.
    parameters :: [(BS.ByteString, BS.ByteString)]
  }

-- | By default, don't set any options during initialization.
defaultConfig :: Config
defaultConfig = Config []

data Handle = Handle
  { -- | A black-box representing the internal state of the solver.
    solver :: ForeignPtr CVC5Solver,
    parser :: ForeignPtr CVC5Parser
  }

foreign import ccall unsafe "cvc5.h cvc5_solver_init" solver_init :: IO (Ptr CVC5Solver)

foreign import ccall unsafe "cvc5.h &cvc5_solver_free" solver_free :: FunPtr (Ptr CVC5Solver -> IO ())

foreign import ccall unsafe "cvc5.h cvc5_solver_set_option" solver_set_option :: Ptr CVC5Solver -> CString -> CString -> IO ()

foreign import ccall unsafe "cvc5.h cvc5_parser_init" parser_init :: Ptr CVC5Solver -> IO (Ptr CVC5Parser)

foreign import ccall unsafe "cvc5.h &cvc5_parser_free" parser_free :: FunPtr (Ptr CVC5Parser -> IO ())

foreign import ccall unsafe "cvc5.h cvc5_eval_smtlib2_string" eval_smtlib2_string :: Ptr CVC5Solver -> Ptr CVC5Parser -> CString -> IO CString

-- | Create a new solver instance.
new :: Config -> IO Handle
new config = do
  slv' <- solver_init
  forM_ (parameters config) $ \(paramId, paramValue) ->
    BS.useAsCString paramId $ \cparamId ->
      BS.useAsCString paramValue $ \cparamValue ->
        solver_set_option slv' cparamId cparamValue
  slv <- newForeignPtr solver_free slv'
  prs' <- parser_init slv'
  prs <- newForeignPtr parser_free prs'

  return $ Handle slv prs

-- | Release the resources associated with a CVC5 instance.
close :: Handle -> IO ()
close h = do
  finalizeForeignPtr $ parser h
  finalizeForeignPtr $ solver h

-- | Create a CVC5 instance, use it to run a computation and release its resources.
with :: Config -> (Handle -> IO a) -> IO a
with config = bracket (new config) close

-- | Create a solver backend out of a CVC5 instance.
toBackend :: Handle -> Backend
toBackend handle = Backend backendSend backendSend_
  where
    backendSend cmd = do
      let slv = solver handle
      let prs = parser handle
      let cmd' =
            LBS.toStrict $
              toLazyByteStringWith
                (untrimmedStrategy smallChunkSize defaultChunkSize)
                "\NUL"
                cmd
      Data.ByteString.Unsafe.unsafeUseAsCString cmd' $ \ccmd' ->
        withForeignPtr slv $ \pslv -> withForeignPtr prs $ \pprs -> do
          resp <- eval_smtlib2_string pslv pprs ccmd'
          LBS.fromStrict <$> BS.packCString resp

    backendSend_ = void . backendSend
