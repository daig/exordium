{-# language MagicHash #-}
-- | Operations on 'ByteArray#'. A 'ByteArray#' is a just a region of raw memory in the garbage-collected heap, which is not scanned for pointers. It carries its own size (in bytes). There are three sets of operations for accessing byte array contents: index for reading from immutable byte arrays, and read/write for mutable byte arrays. Each set contains operations for a range of useful primitive data types. Each operation takes an offset measured in terms of the size of the primitive type being read or written.
module X.Prim.Array.Byte
  (ByteArray#, MutableByteArray#
  -- * Pure
  ,byteArrayContents#
  ,sameMutableByteArray#
  ,sizeofByteArray#, sizeofMutableByteArray#
  -- ** Indexing
  ,indexFloatArray#, indexDoubleArray#
  ,indexStablePtrArray#
  -- *** Char
  ,indexCharArray# ,indexWideCharArray#
  -- *** Int
  ,indexIntArray#, indexInt8Array#, indexInt16Array#, indexInt32Array#, indexInt64Array#
  -- *** Word
  ,indexWordArray#, indexWord8Array#, indexWord16Array#, indexWord32Array#, indexWord64Array#
  -- * IO
  -- ** Initialization
  ,newByteArray#, newPinnedByteArray#, newAlignedPinnedByteArray#
  ,shrinkMutableByteArray#
  ,resizeMutableByteArray#
  ,unsafeFreezeByteArray#
  ,setByteArray#
  -- ** Read
  ,readAddrArray#
  ,readFloatArray#
  ,readDoubleArray#
  ,readStablePtrArray#
  -- *** Char
  ,readCharArray#, readWideCharArray#
  -- *** Int
  ,readIntArray#, readInt8Array#, readInt16Array#, readInt32Array#, readInt64Array#
  -- *** Word
  ,readWordArray#, readWord8Array#, readWord16Array#, readWord32Array#, readWord64Array#
  -- ** Write
  ,writeAddrArray#
  ,writeFloatArray#
  ,writeDoubleArray#
  ,writeStablePtrArray#
  -- *** Char
  ,writeCharArray#, writeWideCharArray#
  -- *** Int
  ,writeIntArray#, writeInt8Array#, writeInt16Array#, writeInt32Array#, writeInt64Array#
  -- *** Word
  ,writeWordArray#, writeWord8Array#, writeWord16Array#, writeWord32Array#, writeWord64Array#
  -- * Move
  ,copyByteArray#, copyMutableByteArray#
  ,copyByteArrayToAddr#, copyMutableByteArrayToAddr#
  ,copyAddrToByteArray#
  -- * Atomic
  ,atomicReadIntArray#, atomicWriteIntArray#
  ,casIntArray#
  ,fetchAddIntArray#, fetchSubIntArray#
  ,fetchAndIntArray#, fetchNandIntArray#
  ,fetchOrIntArray#, fetchXorIntArray#
  -- * Re-exported Types
  , module X
  ) where
import GHC.Prim
import GHC.Prim as X (Addr#,State#,Int#,Char#,StablePtr#,Word#,Float#,Double#)
