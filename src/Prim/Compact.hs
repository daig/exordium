{-# language MagicHash #-}
module Prim.Compact (module X) where
import GHC.Prim as X
  (Compact#, State#, RealWorld, Int#, Word#
  ,compactNew#, compactResize#
  ,compactContains#, compactContainsAny#
  ,compactGetFirstBlock#, compactGetNextBlock#
  ,compactAllocateBlock#
  ,compactFixupPointers#
  ,compactAdd# , compactAddWithSharing#
  ,compactSize#)
