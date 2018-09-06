module U32 where
import qualified GHC.Prim as GHC
import qualified GHC.Word as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize)
import USize (USize#)

type U32 = GHC.Word8

pattern U32# :: USize# -> U32
pattern U32# i = GHC.W8# i


add :: U32 -> U32 -> U32
{-# inline add #-}
add = (GHC.+)

sub :: U32 -> U32 -> U32
{-# inline sub #-}
sub = (GHC.-)

mul :: U32 -> U32 -> U32
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: U32 -> U32 -> U32-}

quot :: U32 -> U32 -> U32
{-# inline quot #-}
quot = GHC.quot

rem :: U32 -> U32 -> U32
{-# inline rem #-}
rem = GHC.rem

quotRem :: U32 -> U32 -> (U32,U32)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: U32 -> U32 -> U32
{-# inline div #-}
div = GHC.div

mod :: U32 -> U32 -> U32
{-# inline mod #-}
mod = GHC.mod

divMod :: U32 -> U32 -> (U32,U32)
{-# inline divMod #-}
divMod = GHC.divMod

and :: U32 -> U32 -> U32
{-# inline and #-}
and = (GHC..&.)

or :: U32 -> U32 -> U32
{-# inline or #-}
or = (GHC..|.)

xor :: U32 -> U32 -> U32
{-# inline xor #-}
xor = GHC.xor

not :: U32 -> U32
{-# inline not #-}
not = (GHC.complement)

negate :: U32 -> U32
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: U32 -> U32 -> (U32,U32)-}
{-subC :: U32 -> U32 -> (U32,U32)-}

gt :: U32 -> U32 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: U32 -> U32 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: U32 -> U32 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: U32 -> U32 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: U32 -> U32 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: U32 -> U32 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: U32 -> ISize -> U32
{-# inline shift #-}
shift = GHC.shift

shiftL :: U32 -> ISize -> U32
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: U32 -> ISize -> U32
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: U32 -> ISize -> U32
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: U32 -> ISize -> U32
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: U32 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: U32 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: U32 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: U32 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> U32
{-# inline bit #-}
bit = GHC.bit
