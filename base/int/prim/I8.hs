-- | To get the size of 'I8#', use @SIZEOF_HSWORD#@ after @#include@ing @"MachDeps.h"@
module I8 (module I8, module X) where
import GHC.Prim
import Types as X

type R = IntRep
type Int = I8#
type MaxBound = 0x7F

pattern MinBound :: I8#
pattern MinBound = -0x80#
minBound () = MinBound

pattern MaxBound :: I8#
pattern MaxBound = 0x7F#
maxBound () = MaxBound

narrow :: Isize# -> I8#
{-# INLINE narrow #-}
narrow = narrow8Int#

add :: I8# -> I8# -> I8#
{-# inline add #-}
add a b = narrow8Int# (a +# b)

sub :: I8# -> I8# -> I8#
{-# inline sub #-}
sub a b = narrow8Int# (a -# b)

mul :: I8# -> I8# -> I8#
{-# inline mul #-}
mul a b = narrow8Int# (a *# b)

{-mulMayOflo :: I8# -> I8# -> I8#-}

quot :: I8# -> I8# -> I8#
{-# inline quot #-}
quot = quotInt#

rem :: I8# -> I8# -> I8#
{-# inline rem #-}
rem = remInt#

quotRem# :: I8# -> I8# -> (# I8#,I8# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt#

and :: I8# -> I8# -> I8#
{-# inline and #-}
and = andI#

or :: I8# -> I8# -> I8#
{-# inline or #-}
or = orI#

xor :: I8# -> I8# -> I8#
{-# inline xor #-}
xor = xorI#

not :: I8# -> I8#
{-# inline not #-}
not = notI#

negate :: I8# -> I8#
{-# inline negate #-}
negate x = narrow8Int# (negateInt# x)

{-addC :: I8# -> I8# -> (# I8#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: I8# -> I8# -> (# I8#,Bool# #)-}
{-subC = subWordC#-}

gt :: I8# -> I8# -> Bool#
{-# inline gt #-}
gt = (>#)

ge :: I8# -> I8# -> Bool#
{-# inline ge #-}
ge = (>=#)

lt :: I8# -> I8# -> Bool#
{-# inline lt #-}
lt = (<#)

le :: I8# -> I8# -> Bool#
{-# inline le #-}
le = (<=#)

eq :: I8# -> I8# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: I8# -> I8# -> Bool#
{-# inline ne #-}
ne = (/=#)

shiftL# :: I8# -> Usize# -> I8#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow8Int# (uncheckedIShiftL# x i)

shiftR# :: I8# -> Usize# -> I8#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow8Int# (uncheckedIShiftRL# x i)
