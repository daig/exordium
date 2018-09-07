module U16 (U16#, module U16) where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @U16#@, but will correctly narrow the return type value.

pattern MinBound :: U16#
pattern MinBound = 0##

pattern MaxBound :: U16#
pattern MaxBound = 0xFFFF##

narrow :: Usize# -> U16#
{-# INLINE narrow #-}
narrow = narrow16Word#

add :: U16# -> U16# -> U16#
{-# inline add #-}
add a b = narrow16Word# (plusWord# a b)

sub :: U16# -> U16# -> U16#
{-# inline sub #-}
sub a b = narrow16Word# (minusWord# a b)

mul :: U16# -> U16# -> U16#
{-# inline mul #-}
mul a b = narrow16Word# (timesWord# a b)

{-mulMayOflo :: U16# -> U16# -> U16#-}

quot :: U16# -> U16# -> U16#
{-# inline quot #-}
quot = quotWord#

rem :: U16# -> U16# -> U16#
{-# inline rem #-}
rem = remWord#

quotRem# :: U16# -> U16# -> (# U16#,U16# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord#

and :: U16# -> U16# -> U16#
{-# inline and #-}
and = and#

or :: U16# -> U16# -> U16#
{-# inline or #-}
or = or#

xor :: U16# -> U16# -> U16#
{-# inline xor #-}
xor = xor#

not :: U16# -> U16#
{-# inline not #-}
not = not#

negate :: U16# -> U16#
{-# inline negate #-}
negate x = narrow16Word# (minusWord# 0## x)

{-addC :: U16# -> U16# -> (# U16#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: U16# -> U16# -> (# U16#,Bool# #)-}
{-subC = subWordC#-}

gt :: U16# -> U16# -> Bool#
{-# inline gt #-}
gt = gtWord#

ge :: U16# -> U16# -> Bool#
{-# inline ge #-}
ge = geWord#

lt :: U16# -> U16# -> Bool#
{-# inline lt #-}
lt = ltWord#

le :: U16# -> U16# -> Bool#
{-# inline le #-}
le = leWord#

eq :: U16# -> U16# -> Bool#
{-# inline eq #-}
eq = eqWord#

ne :: U16# -> U16# -> Bool#
{-# inline ne #-}
ne = neWord#

shiftL# :: U16# -> Usize# -> U16#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow16Word# (uncheckedShiftL# x i)


shiftR# :: U16# -> Usize# -> U16#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow16Word# (uncheckedShiftRL# x i)

popCnt :: U16# -> Usize#
{-# inline popCnt #-}
popCnt = popCnt16#

clz :: U16# -> Usize#
{-# inline clz #-}
clz = clz16#

ctz :: U16# -> Usize#
{-# inline ctz #-}
ctz = ctz16#

pext :: U16# -> U16# -> Usize#
{-# INLINE pext #-}
pext = pext16#

pdep :: U16# -> U16# -> Usize#
{-# INLINE pdep #-}
pdep = pdep16#

byteSwap :: U16# -> U8#
{-# INLINE byteSwap #-}
byteSwap = byteSwap16#
