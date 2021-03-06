{-# language MagicHash #-}
module X.Prim.Float
  (Float#, module X
  ,plusFloat#, minusFloat#, timesFloat#, divideFloat#, powerFloat#
  ,negateFloat# ,fabsFloat#
  ,expFloat#, logFloat#, sqrtFloat#
  ,sinFloat#, cosFloat#, tanFloat#
  ,asinFloat#, acosFloat#, atanFloat#
  ,sinhFloat#, coshFloat#, tanhFloat#
  ,float2Int#, decodeFloat_Int#) where
import GHC.Prim
import X.Prim.Int as X (Int#)

-- | Float literals are written like @3.0#@
