module I32 where
import qualified GHC.Prim as GHC
import qualified GHC.Int as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize,ISize#)

type I32 = GHC.Int32

pattern I32# :: ISize# -> I32
pattern I32# i = GHC.I32# i


add :: I32 -> I32 -> I32
{-# inline add #-}
add = (GHC.+)

sub :: I32 -> I32 -> I32
{-# inline sub #-}
sub = (GHC.-)

mul :: I32 -> I32 -> I32
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: I32 -> I32 -> I32-}

quot :: I32 -> I32 -> I32
{-# inline quot #-}
quot = GHC.quot

rem :: I32 -> I32 -> I32
{-# inline rem #-}
rem = GHC.rem

quotRem :: I32 -> I32 -> (I32,I32)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: I32 -> I32 -> I32
{-# inline div #-}
div = GHC.div

mod :: I32 -> I32 -> I32
{-# inline mod #-}
mod = GHC.mod

divMod :: I32 -> I32 -> (I32,I32)
{-# inline divMod #-}
divMod = GHC.divMod

and :: I32 -> I32 -> I32
{-# inline and #-}
and = (GHC..&.)

or :: I32 -> I32 -> I32
{-# inline or #-}
or = (GHC..|.)

xor :: I32 -> I32 -> I32
{-# inline xor #-}
xor = GHC.xor

not :: I32 -> I32
{-# inline not #-}
not = (GHC.complement)

negate :: I32 -> I32
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: I32 -> I32 -> (I32,I32)-}
{-subC :: I32 -> I32 -> (I32,I32)-}

gt :: I32 -> I32 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: I32 -> I32 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: I32 -> I32 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: I32 -> I32 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: I32 -> I32 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: I32 -> I32 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: I32 -> ISize -> I32
{-# inline shift #-}
shift = GHC.shift

shiftL :: I32 -> ISize -> I32
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: I32 -> ISize -> I32
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: I32 -> ISize -> I32
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: I32 -> ISize -> I32
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: I32 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: I32 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: I32 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: I32 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> I32
{-# inline bit #-}
bit = GHC.bit
