module I16 (module I16, module X) where
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

type I16# = Int#
newtype I16 = I16 GHC.Int16
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern I16# :: I16# -> I16
pattern I16# i = I16 (GHC.I16# i)

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
ne = (GHC./=)

shift :: I16 -> Isize -> I16
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: I16 -> Isize -> I16
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: I16 -> Isize -> I16
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: I16 -> Isize -> I16
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: I16 -> Isize -> I16
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: I16 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: I16 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: I16 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: I16 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> I16
{-# inline bit #-}
bit (Isize i) = GHC.bit i
