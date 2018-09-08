module I64 (module I64, module X) where
import GHC.Prim (Int#)
import qualified GHC.Int as GHC
import GHC.Classes as GHC
import GHC.Num as GHC
import GHC.Real as GHC
import GHC.Enum
import Data.Ix
import Data.Bits as GHC
import GHC.Types as X (Bool)
import GHC.Show
import GHC.Read
import Data.Data
import Foreign.Storable
import Isize

type I64# = Int#
newtype I64 = I64 GHC.Int64
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern I64# :: I64# -> I64
pattern I64# i = I64 (GHC.I64# i)

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
ne = (GHC./=)

shift :: I64 -> Isize -> I64
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: I64 -> Isize -> I64
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: I64 -> Isize -> I64
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: I64 -> Isize -> I64
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: I64 -> Isize -> I64
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: I64 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: I64 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: I64 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: I64 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> I64
{-# inline bit #-}
bit (Isize i) = GHC.bit i
