{-# language MagicHash #-}
module Float.Prim
  (Float#, module X
  ,plusFloat#, minusFloat#, timesFloat#, divideFloat#, powerFloat#
  ,negateFloat# ,fabsFloat#
  ,expFloat#, logFloat#, sqrtFloat#
  ,sinFloat#, cosFloat#, tanFloat#
  ,asinFloat#, acosFloat#, atanFloat#
  ,sinhFloat#, coshFloat#, tanhFloat#
  ,float2Int#, decodeFloat_Int#) where
import GHC.Prim
import Int.Prim as X (Int#)
