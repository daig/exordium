{-# language CPP, PackageImports #-}
module Isize where
import qualified GHC.Prim as GHC
import qualified GHC.Int as GHC
import qualified Prelude as GHC
import qualified Data.Bits as GHC
import GHC.Types (Bool)
import "int-prim" Isize as Prim

type ISize# = GHC.Int#
type ISize = GHC.Int

pattern ISize# :: ISize# -> ISize
pattern ISize# i = GHC.I# i

#define LIFT2(op) op (I64# i) (I64# j) = I64# Prim.op




add :: ISize -> ISize -> ISize
{-# inline add #-}
add = (GHC.+)

sub :: ISize -> ISize -> ISize
{-# inline sub #-}
sub = (GHC.-)

mul :: ISize -> ISize -> ISize
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: ISize -> ISize -> ISize-}

quot :: ISize -> ISize -> ISize
{-# inline quot #-}
quot = GHC.quot

rem :: ISize -> ISize -> ISize
{-# inline rem #-}
rem = GHC.rem

quotRem :: ISize -> ISize -> (ISize,ISize)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: ISize -> ISize -> ISize
{-# inline div #-}
div = GHC.div

mod :: ISize -> ISize -> ISize
{-# inline mod #-}
mod = GHC.mod

divMod :: ISize -> ISize -> (ISize,ISize)
{-# inline divMod #-}
divMod = GHC.divMod

and :: ISize -> ISize -> ISize
{-# inline and #-}
and = (GHC..&.)

or :: ISize -> ISize -> ISize
{-# inline or #-}
or = (GHC..|.)

xor :: ISize -> ISize -> ISize
{-# inline xor #-}
xor = GHC.xor

not :: ISize -> ISize
{-# inline not #-}
not = (GHC.complement)

negate :: ISize -> ISize
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: ISize -> ISize -> (ISize,ISize)-}
{-subC :: ISize -> ISize -> (ISize,ISize)-}

gt :: ISize -> ISize -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: ISize -> ISize -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: ISize -> ISize -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: ISize -> ISize -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: ISize -> ISize -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: ISize -> ISize -> Bool
{-# inline ne #-}
ne = (GHC./=)

shift :: ISize -> ISize -> ISize
{-# inline shift #-}
shift = GHC.shift

shiftL :: ISize -> ISize -> ISize
{-# inline shiftL #-}
shiftL = GHC.shiftL

shiftL# :: ISize -> ISize -> ISize
{-# inline shiftL# #-}
shiftL# = GHC.unsafeShiftL

shiftR# :: ISize -> ISize -> ISize
{-# inline shiftR# #-}
shiftR# = GHC.unsafeShiftR

rotate :: ISize -> ISize -> ISize
{-# inline rotate #-}
rotate = GHC.rotate

popCnt :: ISize -> ISize
{-# inline popCnt #-}
popCnt = GHC.popCount

clz :: ISize -> ISize
{-# inline clz #-}
clz = GHC.countLeadingZeros

ctz :: ISize -> ISize
{-# inline ctz #-}
ctz = GHC.countTrailingZeros

size :: ISize -> ISize
{-# inline size #-}
size = GHC.finiteBitSize

bit :: ISize -> ISize
{-# inline bit #-}
bit = GHC.bit
