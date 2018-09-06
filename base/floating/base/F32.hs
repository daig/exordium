{-# language ForeignFunctionInterface #-}
module F32
  (F32# ,F32(F32#)
  ,NaN32(NaN32#)
  ,maxFinite,epsilon ,minDenormal,minNormal
  ,infinity, nan, nanPayload, getPayload
  ,eq#,sameSignificandBits
  ,succ#,pred#,bisect#
  ,copySign
  ,add
  ) where
import GHC.Prim
import GHC.Types (Int,Word(..),Float(..))
import Debug

type F32# = Float#

newtype F32 = F32_ Float deriving newtype Debug

pattern F32# :: F32# -> F32 
pattern F32# f = F32_ (F# f)

maxFinite = F32# 3.40282347e+38#
epsilon = F32# 1.19209290e-07#

minDenormal = F32# 1e-45#
minNormal = F32# 1.17549435e-38#

infinity :: F32
infinity = F32# (divideFloat# 1.0# 0.0#)

nan :: F32
nan = F32# (divideFloat# 0.0# 0.0#)

foreign import ccall unsafe "identicalf"
  eq# :: F32 -> F32 -> Int

foreign import ccall unsafe "feqrel"
  sameSignificandBits :: F32 -> F32 -> Int

foreign import ccall unsafe "ieeesuccf"
  succ# :: F32 -> F32

foreign import ccall unsafe "ieeepredf"
  pred# :: F32 -> F32

foreign import ccall unsafe "ieeemeanf"
  bisect# :: F32 -> F32 -> F32

foreign import ccall unsafe "copysignf"
  copySign :: F32 -> F32 -> F32


foreign import ccall unsafe "mknanf"
  nanPayload :: NaN32 -> F32

foreign import ccall unsafe "getnanf"
  getPayload :: F32 -> NaN32

-- | 0 to 0x003FFFFF
newtype NaN32 = NaN32_ Word deriving newtype Debug
pattern NaN32# :: Word# -> NaN32
pattern NaN32# w = NaN32_ (W# w)

add (F32# x) (F32# y) = F32# (plusFloat# x y)
