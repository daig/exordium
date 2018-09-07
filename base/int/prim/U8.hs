module U8 (U8#, module U8) where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @U8#@, but will correctly narrow the return type value.

pattern MinBound :: U8#
pattern MinBound = 0##

pattern MaxBound :: U8#
pattern MaxBound = 0xFF##

narrow :: Usize# -> U8#
{-# INLINE narrow #-}
narrow = narrow8Word#

add :: U8# -> U8# -> U8#
{-# inline add #-}
add a b = narrow8Word# (plusWord# a b)

sub :: U8# -> U8# -> U8#
{-# inline sub #-}
sub a b = narrow8Word# (minusWord# a b)

mul :: U8# -> U8# -> U8#
{-# inline mul #-}
mul a b = narrow8Word# (timesWord# a b)

{-mulMayOflo :: U8# -> U8# -> U8#-}

quot :: U8# -> U8# -> U8#
{-# inline quot #-}
quot = quotWord#

rem :: U8# -> U8# -> U8#
{-# inline rem #-}
rem = remWord#

quotRem# :: U8# -> U8# -> (# U8#,U8# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord#

and :: U8# -> U8# -> U8#
{-# inline and #-}
and = and#

or :: U8# -> U8# -> U8#
{-# inline or #-}
or = or#

xor :: U8# -> U8# -> U8#
{-# inline xor #-}
xor = xor#

not :: U8# -> U8#
{-# inline not #-}
not = not#

negate :: U8# -> U8#
{-# inline negate #-}
negate x = narrow8Word# (minusWord# 0## x)

{-addC :: U8# -> U8# -> (# U8#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: U8# -> U8# -> (# U8#,Bool# #)-}
{-subC = subWordC#-}

gt :: U8# -> U8# -> Bool#
{-# inline gt #-}
gt = gtWord#

ge :: U8# -> U8# -> Bool#
{-# inline ge #-}
ge = geWord#

lt :: U8# -> U8# -> Bool#
{-# inline lt #-}
lt = ltWord#

le :: U8# -> U8# -> Bool#
{-# inline le #-}
le = leWord#

eq :: U8# -> U8# -> Bool#
{-# inline eq #-}
eq = eqWord#

ne :: U8# -> U8# -> Bool#
{-# inline ne #-}
ne = neWord#

shiftL# :: U8# -> Usize# -> U8#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow8Word# (uncheckedShiftL# x i)


shiftR# :: U8# -> Usize# -> U8#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow8Word# (uncheckedShiftRL# x i)

popCnt :: U8# -> Usize#
{-# inline popCnt #-}
popCnt = popCnt8#

clz :: U8# -> Usize#
{-# inline clz #-}
clz = clz8#

ctz :: U8# -> Usize#
{-# inline ctz #-}
ctz = ctz8#

pext :: U8# -> U8# -> Usize#
{-# INLINE pext #-}
pext = pext8#

pdep :: U8# -> U8# -> Usize#
{-# INLINE pdep #-}
pdep = pdep8#

{-byteSwap :: U8# -> U8#-}
{-{-# INLINE byteSwap #-}-}
{-byteSwap x = x-}

