{-# language MagicHash #-}

module X.Prim.Int
  (Int# -- | Int literals are written like @3#@
  -- * Arithmetic
  ,(+#),(-#),(*#)
  ,mulIntMayOflo#
  ,quotInt#, remInt#, quotRemInt#
  ,negateInt#
  ,addIntC#, subIntC#
  -- * Comparisons
  ,(>#),(>=#),(==#),(/=#),(<#),(<=#)
  -- * Conversions
  ,int2Addr#, addr2Int#
  ,int2Float#,float2Int#
  ,int2Double#, double2Int#
  ,int2Word#
  ,chr#
  -- ** Narrowing primops
  -- $narrow
  ,narrow8Int#, narrow16Int#, narrow32Int#
  -- * Bitwise Logic
  ,andI#, orI#, xorI#, notI#
  -- * Bitshifts
  ,uncheckedIShiftL#, uncheckedIShiftRA#, uncheckedIShiftRL#
  ) where
import GHC.Prim

-- $narrow
-- #narrow#
