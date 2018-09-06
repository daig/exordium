module U64 where
import qualified GHC.Prim as GHC
import qualified GHC.Word as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize)
import USize (USize#)

type U64 = GHC.Word8

pattern U64# :: USize# -> U64
pattern U64# i = GHC.W8# i


add :: U64 -> U64 -> U64
{-# inline add #-}
add = (GHC.+)

sub :: U64 -> U64 -> U64
{-# inline sub #-}
sub = (GHC.-)

mul :: U64 -> U64 -> U64
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: U64 -> U64 -> U64-}

quot :: U64 -> U64 -> U64
{-# inline quot #-}
quot = GHC.quot

rem :: U64 -> U64 -> U64
{-# inline rem #-}
rem = GHC.rem

quotRem :: U64 -> U64 -> (U64,U64)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: U64 -> U64 -> U64
{-# inline div #-}
div = GHC.div

mod :: U64 -> U64 -> U64
{-# inline mod #-}
mod = GHC.mod

divMod :: U64 -> U64 -> (U64,U64)
{-# inline divMod #-}
divMod = GHC.divMod

and :: U64 -> U64 -> U64
{-# inline and #-}
and = (GHC..&.)

or :: U64 -> U64 -> U64
{-# inline or #-}
or = (GHC..|.)

xor :: U64 -> U64 -> U64
{-# inline xor #-}
xor = GHC.xor

not :: U64 -> U64
{-# inline not #-}
not = (GHC.complement)

negate :: U64 -> U64
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: U64 -> U64 -> (U64,U64)-}
{-subC :: U64 -> U64 -> (U64,U64)-}

gt :: U64 -> U64 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: U64 -> U64 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: U64 -> U64 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: U64 -> U64 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: U64 -> U64 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: U64 -> U64 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: U64 -> ISize -> U64
{-# inline shift #-}
shift = GHC.shift

shiftL :: U64 -> ISize -> U64
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: U64 -> ISize -> U64
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: U64 -> ISize -> U64
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: U64 -> ISize -> U64
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: U64 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: U64 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: U64 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: U64 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> U64
{-# inline bit #-}
bit = GHC.bit
