{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module X.Data.Array.Small (SmallArray(..),type SmallArray#) where
import X.Prim.Array.Small

-- | Small arrays are boxed (im)mutable arrays.
--
-- The underlying structure of the Array type contains a card table, allowing segments of the array to be marked as having been mutated. This allows the garbage collector to only re-traverse segments of the array that have been marked during certain phases, rather than having to traverse the entire array.
--
-- SmallArray lacks this table. This means that it takes up less memory and has slightly faster writes. It is also more efficient during garbage collection so long as the card table would have a single entry covering the entire array. These advantages make them suitable for use as arrays that are known to be small.
--
-- The card size is 128, so for uses much larger than that, Array would likely be superior.
--
-- The underlying type, SmallArray#, was introduced in GHC 7.10, so prior to that version, this module simply implements small arrays as Array.
data SmallArray a = SmallArray# (SmallArray# a)
