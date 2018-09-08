module U32 (module U32, module X) where
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

type U32# = Word#
newtype U32 = U32 GHC.Word32
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern U32# :: U32# -> U32
pattern U32# i = U32 (GHC.W32# i)

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
ne = (GHC./=)

shift :: U32 -> Isize -> U32
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: U32 -> Isize -> U32
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: U32 -> Isize -> U32
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: U32 -> Isize -> U32
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: U32 -> Isize -> U32
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: U32 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: U32 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: U32 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: U32 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> U32
{-# inline bit #-}
bit (Isize i) = GHC.bit i
