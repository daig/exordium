{-# language MagicHash #-}
module Compact.Prim
  (Compact#, module X
  ,compactNew#, compactResize#
  ,compactContains#, compactContainsAny#
  ,compactGetFirstBlock#, compactGetNextBlock#
  ,compactAllocateBlock#
  ,compactFixupPointers#
  ,compactAdd# , compactAddWithSharing#
  ,compactSize#) where
import GHC.Prim
import IO.Prim as X (State#,RealWorld)
import Int.Prim as X (Int#)
import Word.Prim as X(Word#)

