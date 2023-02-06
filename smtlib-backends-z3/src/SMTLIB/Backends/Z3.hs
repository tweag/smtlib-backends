{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module providing a backend that sends commands to Z3 using its C API.
module SMTLIB.Backends.Z3
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
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import SMTLIB.Backends (Backend (..))

data Z3Context

data Z3Config

newtype Config = Config
  { -- | A list of options to set during the solver's initialization.
    -- Each pair is of the form @(paramId, paramValue)@, e.g.
    -- @(":produce-models", "true")@.
    parameters :: [(BS.ByteString, BS.ByteString)]
  }

-- | By default, don't set any options during initialization.
defaultConfig :: Config
defaultConfig = Config []

newtype Handle = Handle
  { -- | A black-box representing the internal state of the solver.
    context :: ForeignPtr Z3Context
  }

foreign import capi unsafe "z3.h &Z3_del_context" c_Z3_del_context :: FunPtr (Ptr Z3Context -> IO ())

foreign import capi unsafe "z3.h Z3_set_param_value" c_Z3_set_param_value :: Ptr Z3Config -> CString -> CString -> IO ()

foreign import capi unsafe "z3.h Z3_mk_config" c_Z3_mk_config :: IO (Ptr Z3Config)

foreign import capi unsafe "z3.h Z3_mk_context" c_Z3_mk_context :: Ptr Z3Config -> IO (Ptr Z3Context)

foreign import capi unsafe "z3.h Z3_del_config" c_Z3_del_config :: Ptr Z3Config -> IO ()

foreign import capi unsafe "z3.h Z3_set_error_handler" c_Z3_set_error_handler :: Ptr Z3Context -> Ptr () -> IO ()

-- We use ccall to avoid warnings about constness in the C side
-- In the meantime we check in cbits/z3.c that the type of the function is
-- compatible.
foreign import ccall unsafe "z3.h Z3_eval_smtlib2_string" c_Z3_eval_smtlib2_string :: Ptr Z3Context -> CString -> IO CString

-- | Create a new solver instance.
new :: Config -> IO Handle
new config = do
  -- we don't set a finalizer for this object as we manually delete it during the
  -- context's creation
  cfg <- c_Z3_mk_config
  forM_ (parameters config) $ \(paramId, paramValue) ->
    BS.useAsCString paramId $ \cparamId ->
      BS.useAsCString paramValue $ \cparamValue ->
        c_Z3_set_param_value cfg cparamId cparamValue
  {-
  We set the error handler to ignore errors. That way if an error happens it doesn't
  cause the whole program to crash, and the error message is simply transmitted to
  the Haskell layer inside the output of the 'send' method.
  -}
  pctx <- c_Z3_mk_context cfg
  c_Z3_del_config cfg
  c_Z3_set_error_handler pctx nullPtr
  ctx <- newForeignPtr c_Z3_del_context pctx
  return $ Handle ctx

-- | Release the resources associated with a Z3 instance.
close :: Handle -> IO ()
close = finalizeForeignPtr . context

-- | Create a Z3 instance, use it to run a computation and release its resources.
with :: Config -> (Handle -> IO a) -> IO a
with config = bracket (new config) close

-- | Create a solver backend out of a Z3 instance.
toBackend :: Handle -> Backend
toBackend handle = Backend backendSend backendSend_
  where
    backendSend cmd = do
      let ctx = context handle
      let cmd' =
            LBS.toStrict $
              toLazyByteStringWith
                (untrimmedStrategy smallChunkSize defaultChunkSize)
                "\NUL"
                cmd
      Data.ByteString.Unsafe.unsafeUseAsCString cmd' $ \ccmd' ->
        withForeignPtr ctx $ \pctx -> do
          resp <- c_Z3_eval_smtlib2_string pctx ccmd'
          LBS.fromStrict <$> BS.packCString resp

    backendSend_ = void . backendSend
