{-# language CPP #-}
-- | Shim for architectures without native 64-bit words
module I64.Shim where
#include "MachDeps.h"
import GHC.Prim
import GHC.Types
#if WORD_SIZE_IN_BITS < 64

import GHC.IntWord64

type Bool# = Int#
type I64# = Int64#
type Isize# = Int#

fromInt :: Isize# -> I64#
{-# INLINE fromInt #-}
fromInt = intToInt64#

toInt :: I64# -> Isize#
{-# INLINE toInt #-}
toInt = int64ToInt#

pattern MinBound :: I64#
pattern MinBound = -0x8000000000000000#

pattern MaxBound :: I64#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#

add :: I64# -> I64# -> I64#
{-# inline add #-}
add = plusInt64#

sub :: I64# -> I64# -> I64#
{-# inline sub #-}
sub = minusInt64#

mul :: I64# -> I64# -> I64#
{-# inline mul #-}
mul = timesInt64#

-- Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
{-mulMayOflo :: I64# -> I64# -> Bool#-}

quot :: I64# -> I64# -> I64#
{-# inline quot #-}
quot = quotInt64#

rem :: I64# -> I64# -> I64#
{-# inline rem #-}
rem = remInt64#

quotRem# :: I64# -> I64# -> (# I64#,I64# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt64#

and :: I64# -> I64# -> I64#
{-# inline and #-}
and (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToInt64# (and64# a b)

or :: I64# -> I64# -> I64#
{-# inline or #-}
or (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToInt64# (or64# a b)

xor :: I64# -> I64# -> I64#
{-# inline xor #-}
xor (int64toWord64# -> a) (int64ToWord64# -> b) = word64ToInt64# (xor64# a b)

not :: I64# -> I64#
{-# inline not #-}
not (int64toWord64# -> a) = word64ToInt64# (not64# a)

negate :: I64# -> I64#
{-# inline negate #-}
negate = negateInt64#

{-addC :: I64# -> I64# -> (# I64#,Bool# #)-}
{-subC :: I64# -> I64# -> (# I64#,Bool# #)-}

gt :: I64# -> I64# -> Bool#
{-# inline gt #-}
gt = gtInt64#

ge :: I64# -> I64# -> Bool#
{-# inline ge #-}
ge = geInt64#

lt :: I64# -> I64# -> Bool#
{-# inline lt #-}
lt = ltInt64#

le :: I64# -> I64# -> Bool#
{-# inline le #-}
le = leInt64#

eq :: I64# -> I64# -> Bool#
{-# inline eq #-}
eq = eqInt64#

ne :: I64# -> I64# -> Bool#
{-# inline ne #-}
ne = neInt64#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: I64# -> Usize# -> I64#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedIShiftL64# x i

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: I64# -> Usize# -> I64#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedIShiftRA64# x i

-- | Shift right logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRL# :: I64# -> Usize# -> I64#
{-# inline shiftRL# #-}
shiftRL# x (word2Int# -> i) = uncheckedIShiftRL64# x i

#endif
