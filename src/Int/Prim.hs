module Int.Prim (module X) where
import GHC.Prim as X
  (Int#
  ,(+#),(-#),(*#)
  ,mulIntMayOflo#
  ,quotInt#, remInt#, quotRemInt#
  ,andI#, orI#, xorI#, notI#
  ,negateInt#
  ,addIntC#, subIntC#
  ,(>#),(>=#),(==#),(/=#),(<#),(<=#)
  ,uncheckedIShiftL#, uncheckedIShiftRA#, uncheckedIShiftRL#
  ,narrow8Int#, narrow16Int#, narrow32Int#)
