-- | Represented natively as Isize
module I64.Native where
import GHC.Prim
import Types

type R = IntRep
type Int = I64#

fromInt :: Isize# -> I64#
{-# INLINE fromInt #-}
fromInt i = i

toInt :: I64# -> Isize#
{-# INLINE toInt #-}
toInt i = i

pattern MinBound :: I64#
pattern MinBound = -0x8000000000000000#

pattern MaxBound :: I64#
pattern MaxBound = 0x7FFFFFFFFFFFFFFF#

add :: I64# -> I64# -> I64#
{-# inline add #-}
add = (+#)

sub :: I64# -> I64# -> I64#
{-# inline sub #-}
sub = (-#)

mul :: I64# -> I64# -> I64#
{-# inline mul #-}
mul = (*#)

-- | Returns 1# if there is any possibility that the upper word of a signed integer multiply might contain useful information. Returns 0# only if there is no possibilyty for overflow to occur.
mulMayOflo :: I64# -> I64# -> Bool#
{-# INLINE mulMayOflo #-}
mulMayOflo = mulIntMayOflo#

quot :: I64# -> I64# -> I64#
{-# inline quot #-}
quot = quotInt#

rem :: I64# -> I64# -> I64#
{-# inline rem #-}
rem = remInt#

quotRem# :: I64# -> I64# -> (# I64#,I64# #)
{-# inline quotRem# #-}
quotRem# = quotRemInt#

-- | Shift left logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftL# :: I64# -> Isize# -> I64#
{-# inline shiftL# #-}
shiftL# = uncheckedIShiftL#

-- | Shift right arithmetic. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftR# :: I64# -> Isize# -> I64#
{-# inline shiftR# #-}
shiftR# = uncheckedIShiftRA#

-- | Shift right logcial. Result is undefined if shift amount @ >= WORD_SIZE_IN_BITS@
shiftRL# :: I64# -> Isize# -> I64#
{-# inline shiftRL# #-}
shiftRL# = uncheckedIShiftRL#

and :: I64# -> I64# -> I64#
{-# inline and #-}
and = andI#

or :: I64# -> I64# -> I64#
{-# inline or #-}
or = orI#

xor :: I64# -> I64# -> I64#
{-# inline xor #-}
xor = xorI#

not :: I64# -> I64#
{-# inline not #-}
not = notI#

negate :: I64# -> I64#
{-# inline negate #-}
negate = negateInt#

addC :: I64# -> I64# -> (# I64#,Bool# #)
{-# INLINE addC #-}
addC = addIntC#

subC :: I64# -> I64# -> (# I64#,Bool# #)
{-# INLINE subC #-}
subC = subIntC#

gt :: I64# -> I64# -> Bool#
{-# inline gt #-}
gt = (<#)

ge :: I64# -> I64# -> Bool#
{-# inline ge #-}
ge = (<=#)

lt :: I64# -> I64# -> Bool#
{-# inline lt #-}
lt = (>#)

le :: I64# -> I64# -> Bool#
{-# inline le #-}
le = (>=#)

eq :: I64# -> I64# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: I64# -> I64# -> Bool#
{-# inline ne #-}
ne = (/=#)
