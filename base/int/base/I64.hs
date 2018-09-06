module I64 where
import qualified GHC.Prim as GHC
import qualified GHC.Int as GHC
import qualified Prelude as GHC
import Data.Bits as GHC
import GHC.Types (Bool)
import ISize (ISize,ISize#)

type I64 = GHC.Int64

pattern I64# :: ISize# -> I64
pattern I64# i = GHC.I64# i


add :: I64 -> I64 -> I64
{-# inline add #-}
add = (GHC.+)

sub :: I64 -> I64 -> I64
{-# inline sub #-}
sub = (GHC.-)

mul :: I64 -> I64 -> I64
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: I64 -> I64 -> I64-}

quot :: I64 -> I64 -> I64
{-# inline quot #-}
quot = GHC.quot

rem :: I64 -> I64 -> I64
{-# inline rem #-}
rem = GHC.rem

quotRem :: I64 -> I64 -> (I64,I64)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: I64 -> I64 -> I64
{-# inline div #-}
div = GHC.div

mod :: I64 -> I64 -> I64
{-# inline mod #-}
mod = GHC.mod

divMod :: I64 -> I64 -> (I64,I64)
{-# inline divMod #-}
divMod = GHC.divMod

and :: I64 -> I64 -> I64
{-# inline and #-}
and = (GHC..&.)

or :: I64 -> I64 -> I64
{-# inline or #-}
or = (GHC..|.)

xor :: I64 -> I64 -> I64
{-# inline xor #-}
xor = GHC.xor

not :: I64 -> I64
{-# inline not #-}
not = (GHC.complement)

negate :: I64 -> I64
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: I64 -> I64 -> (I64,I64)-}
{-subC :: I64 -> I64 -> (I64,I64)-}

gt :: I64 -> I64 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: I64 -> I64 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: I64 -> I64 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: I64 -> I64 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: I64 -> I64 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: I64 -> I64 -> Bool
{-# inline ne #-}
ne = (GHC.==)

shift :: I64 -> ISize -> I64
{-# inline shift #-}
shift = GHC.shift

shiftL :: I64 -> ISize -> I64
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: I64 -> ISize -> I64
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: I64 -> ISize -> I64
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: I64 -> ISize -> I64
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: I64 -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: I64 -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: I64 -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: I64 -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> I64
{-# inline bit #-}
bit = GHC.bit
