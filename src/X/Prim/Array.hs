{-# language MagicHash #-}
module X.Prim.Array
  (Array#, MutableArray#
  -- * Pure
  ,sameMutableArray#
  ,sizeofArray#
  ,sizeofMutableArray#
  ,indexArray#
  -- * IO
  ,newArray#
  ,readArray#, writeArray#
  ,freezeArray#, thawArray#
  ,unsafeFreezeArray#,unsafeThawArray#
  ,copyArray#,copyMutableArray#
  ,cloneArray#, cloneMutableArray#
  -- ** Atomic
  ,casArray#
  -- * Re-exported Types
  ,module X
  ) where
import GHC.Prim
import GHC.Prim as X (Int#,State#)
