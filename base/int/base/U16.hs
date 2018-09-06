module U16 where
import qualified GHC.Prim as GHC
import qualified GHC.Word as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize)
import USize (USize#)

type U16 = GHC.Word8

pattern U16# :: USize# -> U16
pattern U16# i = GHC.W8# i


add :: U16 -> U16 -> U16
{-# inline add #-}
add = (GHC.+)

sub :: U16 -> U16 -> U16
{-# inline sub #-}
sub = (GHC.-)

mul :: U16 -> U16 -> U16
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: U16 -> U16 -> U16-}

quot :: U16 -> U16 -> U16
{-# inline quot #-}
quot = GHC.quot

rem :: U16 -> U16 -> U16
{-# inline rem #-}
rem = GHC.rem

quotRem :: U16 -> U16 -> (U16,U16)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: U16 -> U16 -> U16
{-# inline div #-}
div = GHC.div

mod :: U16 -> U16 -> U16
{-# inline mod #-}
mod = GHC.mod

divMod :: U16 -> U16 -> (U16,U16)
{-# inline divMod #-}
divMod = GHC.divMod

and :: U16 -> U16 -> U16
{-# inline and #-}
and = (GHC..&.)

or :: U16 -> U16 -> U16
{-# inline or #-}
or = (GHC..|.)

xor :: U16 -> U16 -> U16
{-# inline xor #-}
xor = GHC.xor

not :: U16 -> U16
{-# inline not #-}
not = (GHC.complement)

negate :: U16 -> U16
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: U16 -> U16 -> (U16,U16)-}
{-subC :: U16 -> U16 -> (U16,U16)-}

gt :: U16 -> U16 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: U16 -> U16 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: U16 -> U16 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: U16 -> U16 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: U16 -> U16 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: U16 -> U16 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: U16 -> ISize -> U16
{-# inline shift #-}
shift = GHC.shift

shiftL :: U16 -> ISize -> U16
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: U16 -> ISize -> U16
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: U16 -> ISize -> U16
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: U16 -> ISize -> U16
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: U16 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: U16 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: U16 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: U16 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> U16
{-# inline bit #-}
bit = GHC.bit
