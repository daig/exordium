{-# language MagicHash #-}
module Int.Prim
  (Int#
  ,(+#),(-#),(*#)
  ,mulIntMayOflo#
  ,quotInt#, remInt#, quotRemInt#
  ,andI#, orI#, xorI#, notI#
  ,negateInt#
  ,addIntC#, subIntC#
  ,(>#),(>=#),(==#),(/=#),(<#),(<=#)
  ,uncheckedIShiftL#, uncheckedIShiftRA#, uncheckedIShiftRL#
  ,narrow8Int#, narrow16Int#, narrow32Int#) where
import GHC.Prim