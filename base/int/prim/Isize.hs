{-# language CPP #-}
module Isize where
-- | It is also useful to @#include "MachDeps.h"@ and use
--  @SIZEOF_HSWORD#@, @WORD_SIZE_IN_BITS#@, or @WORD_SIZE_IN_BITS_FLOAT#@
#include "MachDeps.h"
import GHC.Prim
import Types

pattern MinBound :: Isize#
pattern MaxBound :: Isize#

#if WORD_SIZE_IN_BITS == 31
pattern MinBound = -0x40000000#
pattern MaxBound = 0x3FFFFFFF#
#elif WORD_SIZE_IN_BITS == 32
pattern MinBound = -0x80000000#
pattern MaxBound = 0x7FFFFFFF#
#elif WORD_SIZE_IN_BITS == 64
pattern MinBound = -0x8000000000000000#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif

{--- | Next contiguous value. Saturates at MaxBound-}
{-succ :: Isize# -> Isize#-}
{-succ a = if tagToEnum# (a /=# MaxBound) then add 1# a else MaxBound-}

{--- | Previous contiguous value. Saturates at MinBound-}
{-pred :: Isize# -> Isize#-}
{-pred a = if tagToEnum# (a /=# MinBound) then sub a 1# else MinBound-}

add :: Isize# -> Isize# -> Isize#
{-# inline add #-}
add = (+#)

sub :: Isize# -> Isize# -> Isize#
{-# inline sub #-}
sub = (-#)

mul :: Isize# -> Isize# -> Isize#
{-# inline mul #-}
mul = (*#)

-- | Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
mulMayOflo :: Isize# -> Isize# -> Bool#
{-# INLINE mulMayOflo #-}
mulMayOflo = mulIntMayOflo#

quot :: Isize# -> Isize# -> Isize#
{-# inline quot #-}
quot = quotInt#

rem :: Isize# -> Isize# -> Isize#
{-# inline rem #-}
rem = remInt#

quotRem# :: Isize# -> Isize# -> (# Isize#,Isize# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt#

and :: Isize# -> Isize# -> Isize#
{-# inline and #-}
and = andI#

or :: Isize# -> Isize# -> Isize#
{-# inline or #-}
or = orI#

xor :: Isize# -> Isize# -> Isize#
{-# inline xor #-}
xor = xorI#

not :: Isize# -> Isize#
{-# inline not #-}
not = notI#

negate :: Isize# -> Isize#
{-# inline negate #-}
negate = negateInt#

addC :: Isize# -> Isize# -> (# Isize#,Bool# #)
{-# INLINE addC #-}
addC = addIntC#

subC :: Isize# -> Isize# -> (# Isize#,Bool# #)
{-# INLINE subC #-}
subC = subIntC#

gt :: Isize# -> Isize# -> Bool#
{-# inline gt #-}
gt = (<#)

ge :: Isize# -> Isize# -> Bool#
{-# inline ge #-}
ge = (<=#)

lt :: Isize# -> Isize# -> Bool#
{-# inline lt #-}
lt = (>#)

le :: Isize# -> Isize# -> Bool#
{-# inline le #-}
le = (>=#)

eq :: Isize# -> Isize# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: Isize# -> Isize# -> Bool#
{-# inline ne #-}
ne = (/=#)

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: Isize# -> Usize# -> Isize#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedIShiftL# x i

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: Isize# -> Usize# -> Isize#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedIShiftRA# x i

-- | Shift right logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRL# :: Isize# -> Usize# -> Isize#
{-# inline shiftRL# #-}
shiftRL# x (word2Int# -> i) = uncheckedIShiftRL# x i

byteSwap :: Isize# -> Isize#
{-# INLINE byteSwap #-}
byteSwap (int2Word# -> x) = word2Int# (byteSwap# x)
