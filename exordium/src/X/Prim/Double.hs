{-# language MagicHash #-}
--------------------------------------------------------------------
-- |
-- Module    :  Prim.Double 
-- Copyright :  (c) <Dai> 2017
-- License   :  MIT
-- Maintainer:  <dailectic@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Double-precision (64 bit) floating-point numbers
--------------------------------------------------------------------
module X.Prim.Double
  (Double#
  -- * Arithmetaic
  ,(+##),(-##),(*##),(/##),(**##)
  ,(>##),(>=##),(==##),(/=##),(<##),(<=##)
  ,negateDouble#
  ,fabsDouble#
  -- Numeric Functions
  ,expDouble#, logDouble#, sqrtDouble#
  ,sinDouble#, cosDouble#, tanDouble#
  ,asinDouble#, acosDouble#, atanDouble#
  ,sinhDouble#, coshDouble#, tanhDouble#
  -- * Conversions
  ,double2Int#, double2Float#
  ,decodeDouble_2Int#, decodeDouble_Int64#
  -- * Re-exported Types
  , module X
  ) where
import GHC.Prim 
import GHC.Prim as X (Int#,Word#,Float#)

-- | Double literals are written like @3.0##@

-- TODO: put documentation for cstring litrals "wow"# somewhere...
