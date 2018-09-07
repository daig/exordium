-- | To get the size of 'Bool#', use @SIZEOF_HSWORD#@ after @#include@ing @"MachDeps.h"@
module Bool (Bool#, module Bool) where
import GHC.Prim
import Types

-- | GHC does not enforce this type, so the operations in this module are unsafe:
-- They assume they are passed a valid @Bool#@, but will correctly narrow the return type value.

pattern False# :: Bool#
pattern False# = 0#

pattern True# :: Bool#
pattern True# = 1#

bool :: a -> a -> Bool# -> a
{-# INLINE bool #-}
bool x y b = if tagToEnum# b then y else x

-- | Next contiguous value. Saturates at MaxBound
succ :: Bool# -> Bool#
succ = orI# 1#

-- | Previous contiguous value. Saturates at MinBound
pred :: Bool# -> Bool#
pred = andI# 0#

{-pattern MinBound :: Bool#-}
{-pattern MinBound = 0#-}

{-pattern MaxBound :: Bool#-}
{-pattern MaxBound = 1#-}

narrow :: Isize# -> Bool#
{-# INLINE narrow #-}
narrow = andI# 1#

{-add :: Bool# -> Bool# -> Bool#-}
{-{-# inline add #-}-}
{-add = xorI#-}

{-sub :: Bool# -> Bool# -> Bool#-}
{-{-# inline sub #-}-}
{-sub = xorI#-}

{-mul :: Bool# -> Bool# -> Bool#-}
{-{-# inline mul #-}-}
{-mul = andI#-}

{-mulMayOflo :: Bool# -> Bool# -> Bool#-}

{-quot :: Bool# -> Bool# -> Bool#-}
{-{-# inline quot #-}-}
{-quot = quotInt#-}

{-rem :: Bool# -> Bool# -> Bool#-}
{-{-# inline rem #-}-}
{-rem = remInt#-}

{-quotRem# :: Bool# -> Bool# -> (# Bool#,Bool# #)-}
{-{-# inline quotRem# #-}-}
{-quotRem# = quotRemInt#-}

and :: Bool# -> Bool# -> Bool#
{-# inline and #-}
and = andI#

or :: Bool# -> Bool# -> Bool#
{-# inline or #-}
or = orI#

xor :: Bool# -> Bool# -> Bool#
{-# inline xor #-}
xor = xorI#

not :: Bool# -> Bool#
{-# inline not #-}
not = notI#

{-negate :: Bool# -> Bool#-}
{-{-# inline negate #-}-}
{-negate x = narrow (negateInt# x)-}

{-addC :: Bool# -> Bool# -> (# Bool#,Bool# #)-}
{-{-# INLINE addC #-}-}
{-addC a b = case plusWord2# a b of (# c, x #) -> (# x, word2Int# c #)-}

{-subC :: Bool# -> Bool# -> (# Bool#,Bool# #)-}
{-subC = subWordC#-}

gt :: Bool# -> Bool# -> Bool#
{-# inline gt #-}
gt = (>#)

ge :: Bool# -> Bool# -> Bool#
{-# inline ge #-}
ge = (>=#)

lt :: Bool# -> Bool# -> Bool#
{-# inline lt #-}
lt = (<#)

le :: Bool# -> Bool# -> Bool#
{-# inline le #-}
le = (<=#)

eq :: Bool# -> Bool# -> Bool#
{-# inline eq #-}
eq = (==#)

ne :: Bool# -> Bool# -> Bool#
{-# inline ne #-}
ne = (/=#)

{-shiftL# :: Bool# -> Usize# -> Bool#-}
{-{-# inline shiftL# #-}-}
{-shiftL# x (word2Int# -> i) = narrow (uncheckedIShiftL# x i)-}

{-shiftR# :: Bool# -> Usize# -> Bool#-}
{-{-# inline shiftR# #-}-}
{-shiftR# x (word2Int# -> i) = narrow (uncheckedIShiftRL# x i)-}
