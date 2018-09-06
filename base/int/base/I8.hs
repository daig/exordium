module I8 where
import qualified GHC.Prim as GHC
import qualified GHC.Int as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize,ISize#)

type I8 = GHC.Int8

pattern I8# :: ISize# -> I8
pattern I8# i = GHC.I8# i


add :: I8 -> I8 -> I8
{-# inline add #-}
add = (GHC.+)

sub :: I8 -> I8 -> I8
{-# inline sub #-}
sub = (GHC.-)

mul :: I8 -> I8 -> I8
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: I8 -> I8 -> I8-}

quot :: I8 -> I8 -> I8
{-# inline quot #-}
quot = GHC.quot

rem :: I8 -> I8 -> I8
{-# inline rem #-}
rem = GHC.rem

quotRem :: I8 -> I8 -> (I8,I8)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: I8 -> I8 -> I8
{-# inline div #-}
div = GHC.div

mod :: I8 -> I8 -> I8
{-# inline mod #-}
mod = GHC.mod

divMod :: I8 -> I8 -> (I8,I8)
{-# inline divMod #-}
divMod = GHC.divMod

and :: I8 -> I8 -> I8
{-# inline and #-}
and = (GHC..&.)

or :: I8 -> I8 -> I8
{-# inline or #-}
or = (GHC..|.)

xor :: I8 -> I8 -> I8
{-# inline xor #-}
xor = GHC.xor

not :: I8 -> I8
{-# inline not #-}
not = (GHC.complement)

negate :: I8 -> I8
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: I8 -> I8 -> (I8,I8)-}
{-subC :: I8 -> I8 -> (I8,I8)-}

gt :: I8 -> I8 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: I8 -> I8 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: I8 -> I8 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: I8 -> I8 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: I8 -> I8 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: I8 -> I8 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: I8 -> ISize -> I8
{-# inline shift #-}
shift = GHC.shift

shiftL :: I8 -> ISize -> I8
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: I8 -> ISize -> I8
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: I8 -> ISize -> I8
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: I8 -> ISize -> I8
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: I8 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: I8 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: I8 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: I8 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> I8
{-# inline bit #-}
bit = GHC.bit
