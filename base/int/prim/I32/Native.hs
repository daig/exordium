-- | To get the size of 'I32#', use @SIZEOF_HSWORD#@ after @#include@ing @"MachDeps.h"@
module I32.Native where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @I32#@, but will correctly narrow the return type value.

type R = IntRep
type Int = I32#

pattern MinBound :: I32#
pattern MinBound = -0x320#

pattern MaxBound :: I32#
pattern MaxBound = 0x7F#

narrow :: Isize# -> I32#
{-# INLINE narrow #-}
narrow = narrow32Int#

add :: I32# -> I32# -> I32#
{-# inline add #-}
add a b = narrow32Int# (a +# b)

sub :: I32# -> I32# -> I32#
{-# inline sub #-}
sub a b = narrow32Int# (a -# b)

mul :: I32# -> I32# -> I32#
{-# inline mul #-}
mul a b = narrow32Int# (a *# b)

{-mulMayOflo :: I32# -> I32# -> I32#-}

quot :: I32# -> I32# -> I32#
{-# inline quot #-}
quot = quotInt#

rem :: I32# -> I32# -> I32#
{-# inline rem #-}
rem = remInt#

quotRem# :: I32# -> I32# -> (# I32#,I32# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt#

and :: I32# -> I32# -> I32#
{-# inline and #-}
and = andI#

or :: I32# -> I32# -> I32#
{-# inline or #-}
or = orI#

xor :: I32# -> I32# -> I32#
{-# inline xor #-}
xor = xorI#

not :: I32# -> I32#
{-# inline not #-}
not = notI#

negate :: I32# -> I32#
{-# inline negate #-}
negate x = narrow32Int# (negateInt# x)

{-addC :: I32# -> I32# -> (# I32#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: I32# -> I32# -> (# I32#,Bool# #)-}
{-subC = subWordC#-}

gt :: I32# -> I32# -> Bool#
{-# inline gt #-}
gt = (>#)

ge :: I32# -> I32# -> Bool#
{-# inline ge #-}
ge = (>=#)

lt :: I32# -> I32# -> Bool#
{-# inline lt #-}
lt = (<#)

le :: I32# -> I32# -> Bool#
{-# inline le #-}
le = (<=#)

eq :: I32# -> I32# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: I32# -> I32# -> Bool#
{-# inline ne #-}
ne = (/=#)

shiftL# :: I32# -> Usize# -> I32#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow32Int# (uncheckedIShiftL# x i)

shiftR# :: I32# -> Usize# -> I32#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow32Int# (uncheckedIShiftRL# x i)

byteSwap :: I32# -> I32#
{-# INLINE byteSwap #-}
byteSwap (int2Word# -> x) = word2Int# (byteSwap32# x)
