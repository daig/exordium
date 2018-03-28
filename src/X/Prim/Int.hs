{-# language MagicHash #-}
module X.Prim.Int
  (Int#
  ,(+#),(-#),(*#)
  ,mulIntMayOflo#
  ,quotInt#, remInt#, quotRemInt#
  ,andI#, orI#, xorI#, notI#
  ,negateInt#
  ,addIntC#, subIntC#
  ,(>#),(>=#),(==#),(/=#),(<#),(<=#)
  ,uncheckedIShiftL#, uncheckedIShiftRA#, uncheckedIShiftRL#
  -- * Narrowing primops
  -- $narrow
  ,narrow8Int#, narrow16Int#, narrow32Int#) where
import GHC.Prim

-- | Int literals are written like @3#@

-- $narrow
-- #narrow#
