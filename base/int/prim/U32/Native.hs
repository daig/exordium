module U32.Native where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @U32#@, but will correctly narrow the return type value.

type R = WordRep
type Word = Usize#

pattern MinBound :: U32#
pattern MinBound = 0##

pattern MaxBound :: U32#
pattern MaxBound = 0xFFFFFFFF##

narrow :: Usize# -> U32#
{-# INLINE narrow #-}
narrow = narrow32Word#

add :: U32# -> U32# -> U32#
{-# inline add #-}
add a b = narrow32Word# (plusWord# a b)

sub :: U32# -> U32# -> U32#
{-# inline sub #-}
sub a b = narrow32Word# (minusWord# a b)

mul :: U32# -> U32# -> U32#
{-# inline mul #-}
mul a b = narrow32Word# (timesWord# a b)

{-mulMayOflo :: U32# -> U32# -> U32#-}

quot :: U32# -> U32# -> U32#
{-# inline quot #-}
quot = quotWord#

rem :: U32# -> U32# -> U32#
{-# inline rem #-}
rem = remWord#

quotRem# :: U32# -> U32# -> (# U32#,U32# #)
{-# inline quotRem# #-}
quotRem# = quotRemWord#

and :: U32# -> U32# -> U32#
{-# inline and #-}
and = and#

or :: U32# -> U32# -> U32#
{-# inline or #-}
or = or#

xor :: U32# -> U32# -> U32#
{-# inline xor #-}
infixl 6 `xor`
xor = xor#

not :: U32# -> U32#
{-# inline not #-}
not = not#

negate :: U32# -> U32#
{-# inline negate #-}
negate x = narrow32Word# (minusWord# 0## x)

{-addC :: U32# -> U32# -> (# U32#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: U32# -> U32# -> (# U32#,Bool# #)-}
{-{-# INLINE subC #-}-}
{-subC = subWordC#-}

gt :: U32# -> U32# -> Bool#
{-# inline gt #-}
gt = gtWord#

ge :: U32# -> U32# -> Bool#
{-# inline ge #-}
ge = geWord#

lt :: U32# -> U32# -> Bool#
{-# inline lt #-}
lt = ltWord#

le :: U32# -> U32# -> Bool#
{-# inline le #-}
le = leWord#

eq :: U32# -> U32# -> Bool#
{-# inline eq #-}
eq = eqWord#

ne :: U32# -> U32# -> Bool#
{-# inline ne #-}
ne = neWord#

shiftL# :: U32# -> Usize# -> U32#
{-# inline shiftL# #-}
shiftL# x (word2Int# -> i) = narrow32Word# (uncheckedShiftL# x i)


shiftR# :: U32# -> Usize# -> U32#
{-# inline shiftR# #-}
shiftR# x (word2Int# -> i) = narrow32Word# (uncheckedShiftRL# x i)

popCnt :: U32# -> Usize#
{-# inline popCnt #-}
popCnt = popCnt32#

clz :: U32# -> Usize#
{-# inline clz #-}
clz = clz32#

ctz :: U32# -> Usize#
{-# inline ctz #-}
ctz = ctz32#

pext :: U32# -> U32# -> Usize#
{-# INLINE pext #-}
pext = pext32#

pdep :: U32# -> U32# -> Usize#
{-# INLINE pdep #-}
pdep = pdep32#

byteSwap :: U32# -> U32#
{-# INLINE byteSwap #-}
byteSwap = byteSwap32#
