{-# language MagicHash #-}
module Prim.Double
  (Double#, module X
  ,(+##),(-##),(*##),(/##),(**##)
  ,(>##),(>=##),(==##),(/=##),(<##),(<=##)
  ,negateDouble#
  ,fabsDouble#
  ,expDouble#, logDouble#, sqrtDouble#
  ,sinDouble#, cosDouble#, tanDouble#
  ,asinDouble#, acosDouble#, atanDouble#
  ,sinhDouble#, coshDouble#, tanhDouble#
  ,double2Int#, double2Float#
  ,decodeDouble_2Int#, decodeDouble_Int64#) where
import GHC.Prim 
import GHC.Prim as X (Int#,Word#,Float#)

-- | Double literals are written like @3.0##@

-- TODO: put documentation for cstring litrals "wow"# somewhere...
