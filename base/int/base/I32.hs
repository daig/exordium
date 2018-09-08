module I32 (module I32, module X) where
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

type I32# = Int#
newtype I32 = I32 GHC.Int32
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern I32# :: I32# -> I32
pattern I32# i = I32 (GHC.I32# i)

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
ne = (GHC./=)

shift :: I32 -> Isize -> I32
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: I32 -> Isize -> I32
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: I32 -> Isize -> I32
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: I32 -> Isize -> I32
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: I32 -> Isize -> I32
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: I32 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: I32 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: I32 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: I32 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> I32
{-# inline bit #-}
bit (Isize i) = GHC.bit i
