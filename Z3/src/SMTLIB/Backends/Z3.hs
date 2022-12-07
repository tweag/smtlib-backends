{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module providing a backend that sends commands to Z3 using its C API.
module SMTLIB.Backends.Z3
  ( Z3 (..),
    new,
    free,
    with,
    toBackend,
  )
where

import Control.Exception (bracket)
import Data.ByteString.Builder.Extra
  ( defaultChunkSize,
    smallChunkSize,
    toLazyByteStringWith,
    untrimmedStrategy,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, newForeignPtr)
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Types as C
import SMTLIB.Backends (Backend (..))

data LogicalContext

C.context
  ( C.baseCtx
      <> C.fptrCtx
      <> C.bsCtx
      <> mempty
        { C.ctxTypesTable =
            M.singleton (C.TypeName "Z3_context") [t|Ptr LogicalContext|]
        }
  )
C.include "z3.h"

data Z3 = Z3
  { -- | A black-box representing the internal state of the solver.
    context :: ForeignPtr LogicalContext
  }

-- | Create a new solver instance.
new :: IO Z3
new = do
  let ctxFinalizer =
        [C.funPtr| void free_context(Z3_context ctx) {
                 Z3_del_context(ctx);
                 } |]

  {-
  We set the error handler to ignore errors. That way if an error happens it doesn't
  cause the whole program to crash, and the error message if simply transmitted to
  the Haskell layer inside the output of the eval_smtlib2_string function.
  -}
  ctx <-
    newForeignPtr ctxFinalizer
      =<< [CU.block| Z3_context {
                 Z3_config cfg = Z3_mk_config();
                 Z3_context ctx = Z3_mk_context(cfg);
                 Z3_del_config(cfg);

                 void ignore_error(Z3_context c, Z3_error_code e) {}
                 Z3_set_error_handler(ctx, ignore_error);

                 return ctx;
                 } |]
  return $ Z3 ctx

-- | Release the resources associated with a Z3 instance.
free :: Z3 -> IO ()
free = finalizeForeignPtr . context

-- | Create a Z3 instance, use it to run a computation and release its resources.
with :: (Z3 -> IO a) -> IO a
with = bracket new free

-- | Create a solver backend out of a Z3 instance.
toBackend :: Z3 -> Backend
toBackend z3 =
  Backend $ \cmd -> do
    let ctx = context z3
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
