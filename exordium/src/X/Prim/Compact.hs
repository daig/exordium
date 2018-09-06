{-# language MagicHash #-}
module X.Prim.Compact
  (Compact#, module X
  ,compactNew#, compactResize#
  ,compactContains#, compactContainsAny#
  ,compactGetFirstBlock#, compactGetNextBlock#
  ,compactAllocateBlock#
  ,compactFixupPointers#
  ,compactAdd# , compactAddWithSharing#
  ,compactSize#) where
import GHC.Prim
import X.Prim.IO as X (State#,RealWorld)
import X.Prim.Int as X (Int#)
import X.Prim.Word as X(Word#)

