{-# language CPP #-}
-- | It is also useful to @#include "MachDeps.h"@ and use
--  @SIZEOF_HSWORD#@, @WORD_SIZE_IN_BITS#@, or @WORD_SIZE_IN_BITS_FLOAT#@
module Usize where
#include "MachDeps.h"
import GHC.Prim
import Types

pattern MinBound :: Usize#
pattern MinBound = 0##

pattern MaxBound :: Usize#
#if WORD_SIZE_IN_BITS == 32
pattern MaxBound = 0xFFFFFFFF##
#elif WORD_SIZE_IN_BITS == 64
pattern MaxBound = 0xFFFFFFFFFFFFFFFF##
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif

{-narrow x = x-}

add :: Usize# -> Usize# -> Usize#
{-# inline add #-}
add = plusWord#

sub :: Usize# -> Usize# -> Usize#
{-# inline sub #-}
sub = minusWord#

mul :: Usize# -> Usize# -> Usize#
{-# inline mul #-}
mul = timesWord#

-- | Returns (# high, low #)
mul2 :: Usize# -> Usize# -> (# Usize# , Usize# #)
{-# INLINE mul2 #-}
mul2 = timesWord2#

{-mulMayOflo :: Usize# -> Usize# -> Usize#-}

quot :: Usize# -> Usize# -> Usize#
{-# inline quot #-}
quot = quotWord#

rem :: Usize# -> Usize# -> Usize#
{-# inline rem #-}
rem = remWord#

quotRem# :: Usize# -> Usize# -> (# Usize#,Usize# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord#

-- | Takes high word of dividend, then low word of dividend, then divisor.
-- Requires that high word is not divisible by divisor.
quotRem2# :: Usize# -> Usize# -> Usize# -> (# Usize#,Usize# #)
{-# inline quotRem2# #-}
quotRem2# = quotRemWord2#

and :: Usize# -> Usize# -> Usize#
{-# inline and #-}
and = and#

or :: Usize# -> Usize# -> Usize#
{-# inline or #-}
or = or#

xor :: Usize# -> Usize# -> Usize#
{-# inline xor #-}
xor = xor#

not :: Usize# -> Usize#
{-# inline not #-}
not = not#

negate :: Usize# -> Usize#
{-# inline negate #-}
negate = minusWord# 0##

-- | Add unsigned integers reporting overflow.
--  The first element of the pair is the result.
--  The second element is the carry flag, which is nonzero on overflow.
--  See also 'add2'.
addC :: Usize# -> Usize# -> (# Usize#,Bool# #)
{-# INLINE addC #-}
addC = addWordC#

-- | Add unsigned integers, with the high part (carry) in the first
--  component of the returned pair and the low part in the second
--  component of the pair.
--  See also 'addC'.
add2 :: Usize# -> Usize# -> (# Usize#, Usize# #)
{-# INLINE add2 #-}
add2 = plusWord2#

subC :: Usize# -> Usize# -> (# Usize#,Bool# #)
subC = subWordC#

gt :: Usize# -> Usize# -> Bool#
{-# inline gt #-}
gt = gtWord#

ge :: Usize# -> Usize# -> Bool#
{-# inline ge #-}
ge = geWord#

lt :: Usize# -> Usize# -> Bool#
{-# inline lt #-}
lt = ltWord#

le :: Usize# -> Usize# -> Bool#
{-# inline le #-}
le = leWord#

eq :: Usize# -> Usize# -> Bool#
{-# inline eq #-}
eq = eqWord#

ne :: Usize# -> Usize# -> Bool#
{-# inline ne #-}
ne = neWord#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: Usize# -> Usize# -> Usize#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = uncheckedShiftL# x i

-- | Shift right logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: Usize# -> Usize# -> Usize#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = uncheckedShiftRL# x i

-- | Count the number of set bits.
popCnt :: Usize# -> Usize#
{-# inline popCnt #-}
popCnt = popCnt#

-- | Count leading zeroes
clz :: Usize# -> Usize#
{-# inline clz #-}
clz = clz#

-- | Count trailing zeroes
ctz :: Usize# -> Usize#
{-# inline ctz #-}
ctz = ctz#

-- | Extract bits from a word at locations specified by a mask
pext :: Usize# -> Usize# -> Usize#
{-# INLINE pext #-}
pext = pext#

-- | Deposit bits to a word at locations specified by a mask
pdep :: Usize# -> Usize# -> Usize#
{-# INLINE pdep #-}
pdep = pdep#

byteSwap :: Usize# -> Usize#
{-# INLINE byteSwap #-}
byteSwap = byteSwap#
