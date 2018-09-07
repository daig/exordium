{-# language CPP #-}
-- | Shim for architectures without native 32-bit words
module U32.Shim where
#include "MachDeps.h"
import GHC.Prim
import GHC.Types
#if WORD_SIZE_IN_BITS < 32

import GHC.IntWord32

type Bool# = Word#
type U32# = Word32#
type Usize# = Word#

fromWord :: Usize# -> U32#
{-# INLINE fromWord #-}
fromWord = intToWord32#

toWord :: U32# -> Usize#
{-# INLINE toWord #-}
toWord = int32ToWord#

pattern MinBound :: U32#
pattern MinBound = -0x8000000000000000#

pattern MaxBound :: U32#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#

add :: U32# -> U32# -> U32#
{-# inline add #-}
add = plusWord32#

sub :: U32# -> U32# -> U32#
{-# inline sub #-}
sub = minusWord32#

mul :: U32# -> U32# -> U32#
{-# inline mul #-}
mul = timesWord32#

-- Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
{-mulMayOflo :: U32# -> U32# -> Bool#-}

quot :: U32# -> U32# -> U32#
{-# inline quot #-}
quot = quotWord32#

rem :: U32# -> U32# -> U32#
{-# inline rem #-}
rem = remWord32#

quotRem# :: U32# -> U32# -> (# U32#,U32# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord32#

and :: U32# -> U32# -> U32#
{-# inline and #-}
and (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToWord32# (and32# a b)

or :: U32# -> U32# -> U32#
{-# inline or #-}
or (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToWord32# (or32# a b)

xor :: U32# -> U32# -> U32#
{-# inline xor #-}
xor (int32toWord32# -> a) (int32ToWord32# -> b) = word32ToWord32# (xor32# a b)

not :: U32# -> U32#
{-# inline not #-}
not (int32toWord32# -> a) = word32ToWord32# (not32# a)

negate :: U32# -> U32#
{-# inline negate #-}
negate = negateWord32#

{-addC :: U32# -> U32# -> (# U32#,Bool# #)-}
{-subC :: U32# -> U32# -> (# U32#,Bool# #)-}

gt :: U32# -> U32# -> Bool#
{-# inline gt #-}
gt = gtWord32#

ge :: U32# -> U32# -> Bool#
{-# inline ge #-}
ge = geWord32#

lt :: U32# -> U32# -> Bool#
{-# inline lt #-}
lt = ltWord32#

le :: U32# -> U32# -> Bool#
{-# inline le #-}
le = leWord32#

eq :: U32# -> U32# -> Bool#
{-# inline eq #-}
eq = eqWord32#

ne :: U32# -> U32# -> Bool#
{-# inline ne #-}
ne = neWord32#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: U32# -> Isize# -> U32#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedShiftL32# x i

-- | Shift right logical. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: U32# -> Usize# -> U32#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedShiftRL32# x i

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRA# :: U32# -> Usize# -> U32#
{-# inline shiftRL# #-}
shiftRA# x (word2Int# -> i) = uncheckedIShiftRA32# x i

#endif
