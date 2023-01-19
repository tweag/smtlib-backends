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
import Data.ByteString.Builder.Extra
  ( defaultChunkSize,
    smallChunkSize,
    toLazyByteStringWith,
    untrimmedStrategy,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default
import qualified Data.Map as M
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, newForeignPtr)
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Types as C
import SMTLIB.Backends (Backend (..))

data Z3Context

data Z3Config

C.context
  ( C.baseCtx
      <> C.fptrCtx
      <> C.bsCtx
      <> mempty
        { C.ctxTypesTable =
            M.singleton (C.TypeName "Z3_context") [t|Ptr Z3Context|]
              <> M.singleton (C.TypeName "Z3_config") [t|Ptr Z3Config|]
        }
  )
C.include "z3.h"

newtype Config = Config
  { -- | A list of options to set during the solver's initialization.
    -- Each pair is of the form @(paramId, paramValue)@, e.g.
    -- @(":produce-models", "true")@.
    parameters :: [(BS.ByteString, BS.ByteString)]
  }

-- | By default, don't set any options during initialization.
defaultConfig :: Config
defaultConfig = Config []

instance Default Config where
  def = defaultConfig

newtype Handle = Handle
  { -- | A black-box representing the internal state of the solver.
    context :: ForeignPtr Z3Context
  }

-- | Create a new solver instance.
new :: Config -> IO Handle
new config = do
  let ctxFinalizer =
        [C.funPtr| void free_context(Z3_context ctx) {
                 Z3_del_context(ctx);
                 } |]
  -- we don't set a finalizer for this object as we manually delete it during the
  -- context's creation
  cfg <- [CU.exp| Z3_config { Z3_mk_config() } |]
  forM_ (parameters config) $ \(paramId, paramValue) ->
    [CU.exp| void { Z3_set_param_value($(Z3_config cfg),
                                       $bs-ptr:paramId,
                                       $bs-ptr:paramValue)
           } |]
  {-
  We set the error handler to ignore errors. That way if an error happens it doesn't
  cause the whole program to crash, and the error message is simply transmitted to
  the Haskell layer inside the output of the 'send' method.
  -}
  ctx <-
    newForeignPtr ctxFinalizer
      =<< [CU.block| Z3_context {
                 Z3_context ctx = Z3_mk_context($(Z3_config cfg));
                 Z3_del_config($(Z3_config cfg));

                 void ignore_error(Z3_context c, Z3_error_code e) {}
                 Z3_set_error_handler(ctx, ignore_error);

                 return ctx;
                 } |]
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
      LBS.fromStrict
        <$> ( BS.packCString
                =<< [CU.exp| const char* {
               Z3_eval_smtlib2_string($fptr-ptr:(Z3_context ctx), $bs-ptr:cmd')
               }|]
            )
    backendSend_ = void . backendSend
