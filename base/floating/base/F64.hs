{-# Language UnliftedFFITypes, ForeignFunctionInterface #-}
-- | https://docs.nvidia.com/cuda/floating-point/index.html
-- https://randomascii.wordpress.com/2012/01/11/tricks-with-the-floating-point-format/
module F64
  (F64# ,F64(F64#)
  ,NaN64(NaN64#)
  ,maxFinite,epsilon ,minDenormal,minNormal
  ,infinity, nan, nanPayload, getPayload
  ,eq#,sameSignificandBits
  ,succ#,pred#,bisect#
  ,copySign
  ,add
  ,module F64
  ) where
import GHC.Prim
import GHC.Types (Int(..),Word(..),Double(..))
import Debug

type F64# = Double#

newtype F64 = F64_ Double deriving newtype Debug

pattern F64# :: F64# -> F64 
pattern F64# f = F64_ (D# f)

pattern F64 {m,e} <- (decodeDouble_Int64# -> (# m,e #)) where F64 = encodeDouble#

foreign import ccall unsafe "ieeeBits" ieeeBits :: F64# -> Word#

decodeDouble# (F64# d) = decodeDouble_Int64# d
decodeDouble (F64# d) = case decodeDouble_Int64# d of (# i, j #) -> (I# i, I# j)
decodeDouble2  (F64# d) = case decodeDouble_2Int# d of (# i, j, k, l #) -> (I# i, W# j, W# k, I# l)

foreign import ccall unsafe "Rts.h __int_encodeDouble"
  encodeDouble# :: Int# -> Int# -> F64#
{-pattern IEEE64# {m :: Int#, e :: Int#} <- (int_decodeDouble# -> b-}
foreign import ccall unsafe "Rts.h __int_encodeDouble"
  encodeDouble :: Int -> Int -> F64

foreign import ccall unsafe "Rts.h __word_encodeDouble"
  encodeDoubleW :: Word -> Int -> F64

maxFinite = F64# 1.7976931348623157e+308##
epsilon = F64# 2.2204460492503131e-16##

minDenormal = F64# 5e-324##
minNormal = F64# 2.2250738585072014e-308##

infinity :: F64
infinity = F64# (1.0## /## 0.0##)

nan :: F64
nan = F64# (0.0## /## 0.0##)

foreign import ccall unsafe "identical"
  eq# :: F64 -> F64 -> Int

foreign import ccall unsafe "feqrel"
  sameSignificandBits :: F64 -> F64 -> Int

foreign import ccall unsafe "ieeesucc"
  succ# :: F64 -> F64

foreign import ccall unsafe "ieeepred"
  pred# :: F64 -> F64

foreign import ccall unsafe "ieeemean"
  bisect# :: F64 -> F64 -> F64

foreign import ccall unsafe "copysign"
  copySign :: F64 -> F64 -> F64


foreign import ccall unsafe "mknan"
  nanPayload :: NaN64 -> F64

foreign import ccall unsafe "getnan"
  getPayload :: F64 -> NaN64

-- | 0 to 0x0007FFFFFFFFFFFF
newtype NaN64 = NaN64_ Word deriving newtype Debug
pattern NaN64# :: Word# -> NaN64
pattern NaN64# w = NaN64_ (W# w)

add (F64# x) (F64# y) = F64# (x +## y)
