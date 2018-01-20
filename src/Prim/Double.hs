{-# language MagicHash #-}
module Prim.Double (module X) where
import GHC.Prim as X
  (Double#, Int#, Word#, Float#
  ,(+##),(-##),(*##),(/##),(**##)
  ,(>##),(>=##),(==##),(/=##),(<##),(<=##)
  ,negateDouble#
  ,fabsDouble#
  ,expDouble#, logDouble#, sqrtDouble#
  ,sinDouble#, cosDouble#, tanDouble#
  ,asinDouble#, acosDouble#, atanDouble#
  ,sinhDouble#, coshDouble#, tanhDouble#
  ,double2Int#, double2Float#
  ,decodeDouble_2Int#, decodeDouble_Int64#)
