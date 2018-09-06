module USize where
import qualified GHC.Prim as GHC
import ISize
import qualified GHC.Word as GHC
import qualified Prelude as GHC
import qualified Data.Bits as GHC
import GHC.Types (Bool)

type USize# = GHC.Word#
type USize = GHC.Word

pattern USize# :: USize# -> USize
pattern USize# i = GHC.W# i


add :: USize -> USize -> USize
{-# inline add #-}
add = (GHC.+)

sub :: USize -> USize -> USize
{-# inline sub #-}
sub = (GHC.-)

mul :: USize -> USize -> USize
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: USize -> USize -> USize-}

quot :: USize -> USize -> USize
{-# inline quot #-}
quot = GHC.quot

rem :: USize -> USize -> USize
{-# inline rem #-}
rem = GHC.rem

quotRem :: USize -> USize -> (USize,USize)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: USize -> USize -> USize
{-# inline div #-}
div = GHC.div

mod :: USize -> USize -> USize
{-# inline mod #-}
mod = GHC.mod

divMod :: USize -> USize -> (USize,USize)
{-# inline divMod #-}
divMod = GHC.divMod

and :: USize -> USize -> USize
{-# inline and #-}
and = (GHC..&.)

or :: USize -> USize -> USize
{-# inline or #-}
or = (GHC..|.)

xor :: USize -> USize -> USize
{-# inline xor #-}
xor = GHC.xor

not :: USize -> USize
{-# inline not #-}
not = (GHC.complement)

negate :: USize -> USize
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: USize -> USize -> (USize,USize)-}
{-subC :: USize -> USize -> (USize,USize)-}

gt :: USize -> USize -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: USize -> USize -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: USize -> USize -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: USize -> USize -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: USize -> USize -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: USize -> USize -> Bool
{-# inline ne #-}
ne = (GHC./=)

shift :: USize -> ISize -> USize
{-# inline shift #-}
shift = GHC.shift

shiftL :: USize -> ISize -> USize
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: USize -> ISize -> USize
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: USize -> ISize -> USize
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: USize -> ISize -> USize
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: USize -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: USize -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: USize -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: USize -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> USize
{-# inline bit #-}
bit = GHC.bit
