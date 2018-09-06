{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module X.Class.Unbox where
import X.Prim.Array.Byte (MutableByteArray#, ByteArray#, State#, Int#, Addr#)
import X.Prim.Int
import X.Type.Int.I
import X.Data.Bool (isTrue#)

-- | Fixed-size unboxed structures which can be packed into ByteArrays.
-- Including primitive types (Int,Word, etc.) and finite product types thereof
class Unbox# a where -- TODO: add Eq constraint
  -- | Size of the type in bytes, ignores its argument
  sizeOf# :: a -> Int#
  -- | Alignment of the type in bytes, ignores its argument
  alignment# :: a -> Int#
  -- | Read a value from the ByteArray at the offset in elements
  indexByteArray# :: ByteArray# -> Int# -> a
  -- | Read a value from the MutableByteArray at the offset in elements
  readByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> (# State# s, a #)
  -- | Write a value to the MutableByteArray at the offset in elements
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  -- | Fill a value into a whole slice of a MutableByteArray, given by an offset and slice length.
  -- By default this will `writeByteArray#` to each position, but faster broadcast methods exist for certain primitive types.
  setByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
  setByteArray# arr off len a = go 0# where
    go i s = if isTrue# (i <# len)
               then case writeByteArray# arr (off +# i) a s of {ss -> go (i +# 1#) ss}
               else s
  indexOffAddr# :: Addr# -> Int# -> a
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
  -- | Fill a value into a whole memory block of an Addr, given by an offset and block length.
  -- By default this will `writeOffAddr#` to each position, but faster broadcast methods exist for certain primitive types.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s
  setOffAddr# addr off len a = go 0# where
    go i s = if isTrue# (i <# len)
             then case writeOffAddr# addr (off +# i) a s of {ss -> go (i +# 1#) ss}
             else s

sizeOf :: forall a. Unbox# a => Int
sizeOf = I# (sizeOf# ((let x = x in x) :: a))
alignment :: forall a. Unbox# a => Int
alignment = I# (alignment# ((let x = x in x) :: a))
