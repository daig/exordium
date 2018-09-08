module U8 (module U8, module X) where
import GHC.Prim (Word#)
import qualified GHC.Int as GHC
import GHC.Classes as GHC
import GHC.Word as GHC
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

type U8# = Word#
newtype U8 = U8 GHC.Word8
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern U8# :: U8# -> U8
pattern U8# i = U8 (GHC.W8# i)

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
ne = (GHC./=)

shift :: U8 -> Isize -> U8
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: U8 -> Isize -> U8
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: U8 -> Isize -> U8
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: U8 -> Isize -> U8
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: U8 -> Isize -> U8
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: U8 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: U8 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: U8 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: U8 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> U8
{-# inline bit #-}
bit (Isize i) = GHC.bit i
