module U64 (module U64, module X) where
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

type U64# = Word#
newtype U64 = U64 GHC.Word64
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern U64# :: U64# -> U64
pattern U64# i = U64 (GHC.W64# i)

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
ne = (GHC./=)

shift :: U64 -> Isize -> U64
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: U64 -> Isize -> U64
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: U64 -> Isize -> U64
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: U64 -> Isize -> U64
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: U64 -> Isize -> U64
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: U64 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: U64 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: U64 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: U64 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> U64
{-# inline bit #-}
bit (Isize i) = GHC.bit i
