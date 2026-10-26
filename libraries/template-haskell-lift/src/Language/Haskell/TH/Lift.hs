{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

module Language.Haskell.TH.Lift
  ( Lift(..)
  , Q
  , Code
  , Quote
  , Exp
  -- * Compatibilty shims
  , defaultLiftTyped
  , liftAddrCompat
  , liftIntCompat
  ) where

import GHC.Exts (Int(..))
import Data.Word (Word8)
#if  __GLASGOW_HASKELL__ < 810
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
#endif
import Foreign.ForeignPtr (ForeignPtr)
#if __GLASGOW_HASKELL__ >= 912
import GHC.Internal.TH.Lift
import GHC.Internal.TH.Syntax
#else
import Language.Haskell.TH.Syntax
#endif
#if __GLASGOW_HASKELL__ >= 915
import GHC.Internal.TH.Monad
#endif


-- Quote and Code are introduced in GHC-9.0/template-haskell-2.17
-- If we are using an older version, it is basically synonymous with 'Q'
#if __GLASGOW_HASKELL__ < 900
type Quote m = (Q ~ m)
type Code m a = m (TExp a)
#endif

-- | Convert an implementation of 'lift' into one for 'liftTyped'.
#if  __GLASGOW_HASKELL__ >= 900
defaultLiftTyped :: (Lift a, Quote m) => a -> Code m a
defaultLiftTyped x = unsafeCodeCoerce (lift x)
-- template-haskell >= 2.17
#else
defaultLiftTyped :: (Lift a, Quote m) => a -> Q (TExp a)
defaultLiftTyped x = unsafeTExpCoerce (lift x)
-- template-haskell >= 2.16
#endif

-- | A compatibility shim for lifting an 'Addr#' value.
liftAddrCompat :: Quote m => ForeignPtr Word8 -> Word -> Word -> m Exp
liftAddrCompat fptr off len =
#if  __GLASGOW_HASKELL__ >= 810
     pure $ LitE $ BytesPrimL $ Bytes fptr off len
#else
     do
       let
         loop !ptr 0 xs = pure $ reverse xs
         loop !ptr !len xs = do
           x <- peek ptr
           loop (ptr `plusPtr` 1) (len -1) (x:xs)
       let words = unsafePerformIO $ withForeignPtr fptr $ \ptr -> loop (ptr `plusPtr` (fromIntegral off)) len []
       pure $ LitE $ StringPrimL $ words
#endif

-- | A compatibility shim for lifting an 'Int' value without triggering bugs if @RebindableSyntax@ is used in a module.
liftIntCompat :: Quote m => Integer -> m Exp
liftIntCompat n = pure $ AppE (ConE 'I#) (LitE (IntPrimL n))
