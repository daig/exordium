{-# language MagicHash #-}
module Prim.Float
  (Float#, module X
  ,plusFloat#, minusFloat#, timesFloat#, divideFloat#, powerFloat#
  ,negateFloat# ,fabsFloat#
  ,expFloat#, logFloat#, sqrtFloat#
  ,sinFloat#, cosFloat#, tanFloat#
  ,asinFloat#, acosFloat#, atanFloat#
  ,sinhFloat#, coshFloat#, tanhFloat#
  ,float2Int#, decodeFloat_Int#) where
import GHC.Prim
import Prim.Int as X (Int#)
