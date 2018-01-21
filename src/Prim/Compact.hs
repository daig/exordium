{-# language MagicHash #-}
module Prim.Compact
  (Compact#, module X
  ,compactNew#, compactResize#
  ,compactContains#, compactContainsAny#
  ,compactGetFirstBlock#, compactGetNextBlock#
  ,compactAllocateBlock#
  ,compactFixupPointers#
  ,compactAdd# , compactAddWithSharing#
  ,compactSize#) where
import GHC.Prim
import Prim.IO as X (State#,RealWorld)
import Prim.Int as X (Int#)
import Prim.Word as X(Word#)

