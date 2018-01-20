{-# language MagicHash #-}
module Prim.Float (module X) where
import GHC.Prim as X
  (Float#, Int#
  ,plusWord#, minusWord#, timesWord#, divideFloat#, powerFloat#
  ,negateFloat# ,fabsFloat#
  ,expFloat#, logFloat#, sqrtFloat#
  ,sinFloat#, cosFloat#, tanFloat#
  ,asinFloat#, acosFloat#, atanFloat#
  ,sinhFloat#, coshFloat#, tanhFloat#
  ,float2Int#, decodeFloat_Int#)
