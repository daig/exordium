{-# language MagicHash #-}
-- | Operations on 'ArrayArray#'. An 'ArrayArray#' contains references to /unpointed/ arrays, such as 'X.Prim.Array.Byte.ByteArray#'s. Hence, it is not parameterised by the element types, just like a 'X.Prim.Array.Byte.ByteArray#', but it needs to be scanned during GC, just like an 'X.Prim.Array.Array#'. We represent an 'ArrayArray#' exactly as a 'X.Prim.Array.Array#', but provide element-type-specific indexing, reading, and writing.
module X.Prim.Array.Array
  (ArrayArray#, MutableArrayArray#
  -- * Pure
  ,sameMutableArrayArray#
  ,sizeofMutableArrayArray#
  -- ** Indexing
  ,indexByteArrayArray#, indexArrayArrayArray#
  -- * IO
  -- ** Initialize
  ,newArrayArray#
  ,unsafeFreezeArrayArray#
  -- ** Read
  ,readByteArrayArray#, readMutableByteArrayArray#, readArrayArrayArray#
  -- ** Write
  ,writeByteArrayArray#, writeMutableByteArrayArray#, writeArrayArrayArray#
  -- ** Move
  ,copyArrayArray#, copyMutableArrayArray#
  -- * Re-exported Types
  ,module X
  ) where
import GHC.Prim
import GHC.Prim as X (State#,Int#)
