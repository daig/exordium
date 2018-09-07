{-# language CPP #-}
-- | Shim for architectures without native 64-bit words
module U64.Shim where
#include "MachDeps.h"
import GHC.Prim
import Types
#if WORD_SIZE_IN_BITS < 64

import GHC.IntWord64

fromWord :: Usize# -> U64#
{-# INLINE fromWord #-}
fromWord = intToWord64#

toWord :: U64# -> Usize#
{-# INLINE toWord #-}
toWord = int64ToWord#

pattern MinBound :: U64#
pattern MinBound = -0x8000000000000000#

pattern MaxBound :: U64#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#

add :: U64# -> U64# -> U64#
{-# inline add #-}
add = plusWord64#

sub :: U64# -> U64# -> U64#
{-# inline sub #-}
sub = minusWord64#

mul :: U64# -> U64# -> U64#
{-# inline mul #-}
mul = timesWord64#

-- Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
{-mulMayOflo :: U64# -> U64# -> Bool#-}

quot :: U64# -> U64# -> U64#
{-# inline quot #-}
quot = quotWord64#

rem :: U64# -> U64# -> U64#
{-# inline rem #-}
rem = remWord64#

quotRem# :: U64# -> U64# -> (# U64#,U64# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord64#

and :: U64# -> U64# -> U64#
{-# inline and #-}
and (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToWord64# (and64# a b)

or :: U64# -> U64# -> U64#
{-# inline or #-}
or (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToWord64# (or64# a b)

xor :: U64# -> U64# -> U64#
{-# inline xor #-}
xor (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToWord64# (xor64# a b)

not :: U64# -> U64#
{-# inline not #-}
not (int64toWord64# -> a) = word64ToWord64# (not64# a)

negate :: U64# -> U64#
{-# inline negate #-}
negate = negateWord64#

{-addC :: U64# -> U64# -> (# U64#,Bool# #)-}
{-subC :: U64# -> U64# -> (# U64#,Bool# #)-}

gt :: U64# -> U64# -> Bool#
{-# inline gt #-}
gt = gtWord64#

ge :: U64# -> U64# -> Bool#
{-# inline ge #-}
ge = geWord64#

lt :: U64# -> U64# -> Bool#
{-# inline lt #-}
lt = ltWord64#

le :: U64# -> U64# -> Bool#
{-# inline le #-}
le = leWord64#

eq :: U64# -> U64# -> Bool#
{-# inline eq #-}
eq = eqWord64#

ne :: U64# -> U64# -> Bool#
{-# inline ne #-}
ne = neWord64#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: U64# -> Isize# -> U64#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedShiftL64# x i

-- | Shift right logical. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: U64# -> Usize# -> U64#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedShiftRL64# x i

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRA# :: U64# -> Usize# -> U64#
{-# inline shiftRL# #-}
shiftRA# x (word2Int# -> i) = uncheckedIShiftRA64# x i

#endif
