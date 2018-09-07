module U64.Native where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @U64#@, but will correctly narrow the return type value.

pattern MinBound :: U64#
pattern MinBound = 0##

pattern MaxBound :: U64#
pattern MaxBound = 0xFFFFFFFFFFFFFFFF##

narrow :: Usize# -> U64#
{-# INLINE narrow #-}
narrow = narrow32Word#

add :: U64# -> U64# -> U64#
{-# inline add #-}
add a b = narrow32Word# (plusWord# a b)

sub :: U64# -> U64# -> U64#
{-# inline sub #-}
sub a b = narrow32Word# (minusWord# a b)

mul :: U64# -> U64# -> U64#
{-# inline mul #-}
mul a b = narrow32Word# (timesWord# a b)

{-mulMayOflo :: U64# -> U64# -> U64#-}

quot :: U64# -> U64# -> U64#
{-# inline quot #-}
quot = quotWord#

rem :: U64# -> U64# -> U64#
{-# inline rem #-}
rem = remWord#

quotRem# :: U64# -> U64# -> (# U64#,U64# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord#

and :: U64# -> U64# -> U64#
{-# inline and #-}
and = and#

or :: U64# -> U64# -> U64#
{-# inline or #-}
or = or#

xor :: U64# -> U64# -> U64#
{-# inline xor #-}
xor = xor#

not :: U64# -> U64#
{-# inline not #-}
not = not#

negate :: U64# -> U64#
{-# inline negate #-}
negate = minusWord# 0##

addC :: U64# -> U64# -> (# U64#,Bool# #)
{-# INLINE addC #-}
addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)

subC :: U64# -> U64# -> (# U64#,Bool# #)
{-# INLINE subC #-}
subC = subWordC#

gt :: U64# -> U64# -> Bool#
{-# inline gt #-}
gt = gtWord#

ge :: U64# -> U64# -> Bool#
{-# inline ge #-}
ge = geWord#

lt :: U64# -> U64# -> Bool#
{-# inline lt #-}
lt = ltWord#

le :: U64# -> U64# -> Bool#
{-# inline le #-}
le = leWord#

eq :: U64# -> U64# -> Bool#
{-# inline eq #-}
eq = eqWord#

ne :: U64# -> U64# -> Bool#
{-# inline ne #-}
ne = neWord#

shiftL# :: U64# -> Usize# -> U64#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow32Word# (uncheckedShiftL# x i)


shiftR# :: U64# -> Usize# -> U64#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow32Word# (uncheckedShiftRL# x i)

popCnt :: U64# -> Usize#
{-# inline popCnt #-}
popCnt = popCnt32#

clz :: U64# -> Usize#
{-# inline clz #-}
clz = clz32#

ctz :: U64# -> Usize#
{-# inline ctz #-}
ctz = ctz32#

pext :: U64# -> U64# -> Usize#
{-# INLINE pext #-}
pext = pext32#

pdep :: U64# -> U64# -> Usize#
{-# INLINE pdep #-}
pdep = pdep32#

byteSwap :: U64# -> U64#
{-# INLINE byteSwap #-}
byteSwap = byteSwap64#
