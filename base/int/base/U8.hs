module U8 where
import qualified GHC.Prim as GHC
import qualified GHC.Word as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize)
import USize (USize#)

type U8 = GHC.Word8

pattern U8# :: USize# -> U8
pattern U8# i = GHC.W8# i


add :: U8 -> U8 -> U8
{-# inline add #-}
add = (GHC.+)

sub :: U8 -> U8 -> U8
{-# inline sub #-}
sub = (GHC.-)

mul :: U8 -> U8 -> U8
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: U8 -> U8 -> U8-}

quot :: U8 -> U8 -> U8
{-# inline quot #-}
quot = GHC.quot

rem :: U8 -> U8 -> U8
{-# inline rem #-}
rem = GHC.rem

quotRem :: U8 -> U8 -> (U8,U8)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: U8 -> U8 -> U8
{-# inline div #-}
div = GHC.div

mod :: U8 -> U8 -> U8
{-# inline mod #-}
mod = GHC.mod

divMod :: U8 -> U8 -> (U8,U8)
{-# inline divMod #-}
divMod = GHC.divMod

and :: U8 -> U8 -> U8
{-# inline and #-}
and = (GHC..&.)

or :: U8 -> U8 -> U8
{-# inline or #-}
or = (GHC..|.)

xor :: U8 -> U8 -> U8
{-# inline xor #-}
xor = GHC.xor

not :: U8 -> U8
{-# inline not #-}
not = (GHC.complement)

negate :: U8 -> U8
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: U8 -> U8 -> (U8,U8)-}
{-subC :: U8 -> U8 -> (U8,U8)-}

gt :: U8 -> U8 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: U8 -> U8 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: U8 -> U8 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: U8 -> U8 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: U8 -> U8 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: U8 -> U8 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: U8 -> ISize -> U8
{-# inline shift #-}
shift = GHC.shift

shiftL :: U8 -> ISize -> U8
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: U8 -> ISize -> U8
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: U8 -> ISize -> U8
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: U8 -> ISize -> U8
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: U8 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: U8 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: U8 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: U8 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> U8
{-# inline bit #-}
bit = GHC.bit
