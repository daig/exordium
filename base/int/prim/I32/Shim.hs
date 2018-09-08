{-# language CPP #-}
-- | Shim for architectures without native 32-bit words
module I32.Shim where
#include "MachDeps.h"
import GHC.Prim
import GHC.Types
#if WORD_SIZE_IN_BITS < 32
import GHC.IntWord32

type R = IntRep
type Int = I32#


fromInt :: Isize# -> I32#
{-# INLINE fromInt #-}
fromInt = intToInt32#

toInt :: I32# -> Isize#
{-# INLINE toInt #-}
toInt = int32ToInt#

pattern MinBound :: I32#
pattern MinBound = -0x8000000000000000#

pattern MaxBound :: I32#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#

add :: I32# -> I32# -> I32#
{-# inline add #-}
add = plusInt32#

sub :: I32# -> I32# -> I32#
{-# inline sub #-}
sub = minusInt32#

mul :: I32# -> I32# -> I32#
{-# inline mul #-}
mul = timesInt32#

-- Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
{-mulMayOflo :: I32# -> I32# -> Bool#-}

quot :: I32# -> I32# -> I32#
{-# inline quot #-}
quot = quotInt32#

rem :: I32# -> I32# -> I32#
{-# inline rem #-}
rem = remInt32#

quotRem# :: I32# -> I32# -> (# I32#,I32# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt32#

and :: I32# -> I32# -> I32#
{-# inline and #-}
and (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToInt32# (and32# a b)

or :: I32# -> I32# -> I32#
{-# inline or #-}
or (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToInt32# (or32# a b)

xor :: I32# -> I32# -> I32#
{-# inline xor #-}
xor (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToInt32# (xor32# a b)

not :: I32# -> I32#
{-# inline not #-}
not (int32toWord32# -> a) = word32ToInt32# (not32# a)

negate :: I32# -> I32#
{-# inline negate #-}
negate = negateInt32#

{-addC :: I32# -> I32# -> (# I32#,Bool# #)-}
{-subC :: I32# -> I32# -> (# I32#,Bool# #)-}

gt :: I32# -> I32# -> Bool#
{-# inline gt #-}
gt = gtInt32#

ge :: I32# -> I32# -> Bool#
{-# inline ge #-}
ge = geInt32#

lt :: I32# -> I32# -> Bool#
{-# inline lt #-}
lt = ltInt32#

le :: I32# -> I32# -> Bool#
{-# inline le #-}
le = leInt32#

eq :: I32# -> I32# -> Bool#
{-# inline eq #-}
eq = eqInt32#

ne :: I32# -> I32# -> Bool#
{-# inline ne #-}
ne = neInt32#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: I32# -> Usize# -> I32#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedIShiftL32# x i

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: I32# -> Usize# -> I32#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedIShiftRA32# x i

-- | Shift right logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRL# :: I32# -> Usize# -> I32#
{-# inline shiftRL# #-}
shiftRL# x (word2Int# -> i) = uncheckedIShiftRL32# x i

#endif
