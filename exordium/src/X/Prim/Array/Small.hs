{-# language MagicHash #-}
-- | A 'SmallArray#' works just like an 'X.Prim.Array.Array#', but with different space use and performance characteristics (that are often useful with small arrays). The SmallArray# and SmallMutableArray# lack a `card table'. The purpose of a card table is to avoid having to scan every element of the array on each GC by keeping track of which elements have changed since the last GC and only scanning those that have changed. So the consequence of there being no card table is that the representation is somewhat smaller and the writes are somewhat faster (because the card table does not need to be updated). The disadvantage of course is that for a 'SmallMutableArray#' the whole array has to be scanned on each GC. Thus it is best suited for use cases where the mutable array is not long lived, e.g. where a mutable array is initialised quickly and then frozen to become an immutable 'SmallArray#'.
module X.Prim.Array.Small
  (SmallArray#, SmallMutableArray#
  ,newSmallArray#
  ,sameSmallMutableArray#
  ,readSmallArray#, writeSmallArray#
  ,sizeofSmallArray#
  ,sizeofSmallMutableArray#
  ,indexSmallArray#
  ,unsafeFreezeSmallArray#,unsafeThawSmallArray#
  ,copySmallArray#,copySmallMutableArray#
  ,cloneSmallArray#, cloneSmallMutableArray#
  ,freezeSmallArray#, thawSmallArray#
  ,casSmallArray#
  -- * Re-exported Types
  ,module X
  ) where
import GHC.Prim
import GHC.Prim as X (Int#,State#)
