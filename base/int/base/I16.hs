module I16 where
import qualified GHC.Prim as GHC
import qualified GHC.Int as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize,ISize#)

type I16 = GHC.Int16

pattern I16# :: ISize# -> I16
pattern I16# i = GHC.I16# i


add :: I16 -> I16 -> I16
{-# inline add #-}
add = (GHC.+)

sub :: I16 -> I16 -> I16
{-# inline sub #-}
sub = (GHC.-)

mul :: I16 -> I16 -> I16
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: I16 -> I16 -> I16-}

quot :: I16 -> I16 -> I16
{-# inline quot #-}
quot = GHC.quot

rem :: I16 -> I16 -> I16
{-# inline rem #-}
rem = GHC.rem

quotRem :: I16 -> I16 -> (I16,I16)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: I16 -> I16 -> I16
{-# inline div #-}
div = GHC.div

mod :: I16 -> I16 -> I16
{-# inline mod #-}
mod = GHC.mod

divMod :: I16 -> I16 -> (I16,I16)
{-# inline divMod #-}
divMod = GHC.divMod

and :: I16 -> I16 -> I16
{-# inline and #-}
and = (GHC..&.)

or :: I16 -> I16 -> I16
{-# inline or #-}
or = (GHC..|.)

xor :: I16 -> I16 -> I16
{-# inline xor #-}
xor = GHC.xor

not :: I16 -> I16
{-# inline not #-}
not = (GHC.complement)

negate :: I16 -> I16
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: I16 -> I16 -> (I16,I16)-}
{-subC :: I16 -> I16 -> (I16,I16)-}

gt :: I16 -> I16 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: I16 -> I16 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: I16 -> I16 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: I16 -> I16 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: I16 -> I16 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: I16 -> I16 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: I16 -> ISize -> I16
{-# inline shift #-}
shift = GHC.shift

shiftL :: I16 -> ISize -> I16
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: I16 -> ISize -> I16
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: I16 -> ISize -> I16
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: I16 -> ISize -> I16
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: I16 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: I16 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: I16 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: I16 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> I16
{-# inline bit #-}
bit = GHC.bit
