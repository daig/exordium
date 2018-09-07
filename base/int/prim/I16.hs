-- | To get the size of 'I16#', use @SIZEOF_HSWORD#@ after @#include@ing @"MachDeps.h"@
module I16 (I16#, module I16) where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @I16#@, but will correctly narrow the return type value.

pattern MinBound :: I16#
pattern MinBound = -0x80#

pattern MaxBound :: I16#
pattern MaxBound = 0x7F#

narrow :: Isize# -> I16#
{-# INLINE narrow #-}
narrow = narrow16Int#

add :: I16# -> I16# -> I16#
{-# inline add #-}
add a b = narrow16Int# (a +# b)

sub :: I16# -> I16# -> I16#
{-# inline sub #-}
sub a b = narrow16Int# (a -# b)

mul :: I16# -> I16# -> I16#
{-# inline mul #-}
mul a b = narrow16Int# (a *# b)

{-mulMayOflo :: I16# -> I16# -> I16#-}

quot :: I16# -> I16# -> I16#
{-# inline quot #-}
quot = quotInt#

rem :: I16# -> I16# -> I16#
{-# inline rem #-}
rem = remInt#

quotRem# :: I16# -> I16# -> (# I16#,I16# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt#

and :: I16# -> I16# -> I16#
{-# inline and #-}
and = andI#

or :: I16# -> I16# -> I16#
{-# inline or #-}
or = orI#

xor :: I16# -> I16# -> I16#
{-# inline xor #-}
xor = xorI#

not :: I16# -> I16#
{-# inline not #-}
not = notI#

negate :: I16# -> I16#
{-# inline negate #-}
negate x = narrow16Int# (negateInt# x)

{-addC :: I16# -> I16# -> (# I16#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: I16# -> I16# -> (# I16#,Bool# #)-}
{-subC = subWordC#-}

gt :: I16# -> I16# -> Bool#
{-# inline gt #-}
gt = (>#)

ge :: I16# -> I16# -> Bool#
{-# inline ge #-}
ge = (>=#)

lt :: I16# -> I16# -> Bool#
{-# inline lt #-}
lt = (<#)

le :: I16# -> I16# -> Bool#
{-# inline le #-}
le = (<=#)

eq :: I16# -> I16# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: I16# -> I16# -> Bool#
{-# inline ne #-}
ne = (/=#)

shiftL# :: I16# -> Usize# -> I16#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow16Int# (uncheckedIShiftL# x i)

shiftR# :: I16# -> Usize# -> I16#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow16Int# (uncheckedIShiftRL# x i)

byteSwap :: I16# -> I16#
{-# INLINE byteSwap #-}
byteSwap (int2Word# -> x) = word2Int# (byteSwap16# x)
