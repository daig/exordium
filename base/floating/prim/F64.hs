{-# language UnliftedFFITypes, ForeignFunctionInterface #-}
module F64 where
import GHC.Prim
import GHC.Types (RuntimeRep(..))
import Foreign.Ptr
import GHC.Types

type R = DoubleRep
type F64# = Double#

foreign import ccall unsafe "Rts.h __int_encodeDouble"
  int_encodeDouble# :: Int -> Int -> Double

{-foreign import ccall unsafe "__encodeDouble"-}
  {-encoudeDouble# :: Int# -> ByteArray# -> Int# -> F64#-}

foreign import ccall unsafe "getMonotonicNSec"
  getTime :: IO Word
{-foreign import ccall unsafe "aaaa"-}
  {-newSpark# :: Ptr () -> Ptr () -> Int#-}
