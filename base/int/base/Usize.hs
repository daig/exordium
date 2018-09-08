module Usize (module Usize, module X) where
import GHC.Prim (Word#)
import qualified GHC.Word as GHC
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

type Usize# = Word#
newtype Usize = Usize GHC.Word
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern U# :: Usize# -> Usize
pattern U# i = Usize (GHC.W# i)

add :: Usize -> Usize -> Usize
{-# inline add #-}
add = (GHC.+)

sub :: Usize -> Usize -> Usize
{-# inline sub #-}
sub = (GHC.-)

mul :: Usize -> Usize -> Usize
{-# inline mul #-}
mul = (GHC.*)

{-negate = GHC.negate-}
{-mulMayOflo :: Usize -> Usize -> Usize-}

quot :: Usize -> Usize -> Usize
{-# inline quot #-}
quot = GHC.quot

rem :: Usize -> Usize -> Usize
{-# inline rem #-}
rem = GHC.rem

quotRem :: Usize -> Usize -> (Usize,Usize)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: Usize -> Usize -> Usize
{-# inline div #-}
div = GHC.div

mod :: Usize -> Usize -> Usize
{-# inline mod #-}
mod = GHC.mod

divMod :: Usize -> Usize -> (Usize,Usize)
{-# inline divMod #-}
divMod = GHC.divMod

and :: Usize -> Usize -> Usize
{-# inline and #-}
and = (GHC..&.)

or :: Usize -> Usize -> Usize
{-# inline or #-}
or = (GHC..|.)

xor :: Usize -> Usize -> Usize
{-# inline xor #-}
xor = GHC.xor

not :: Usize -> Usize
{-# inline not #-}
not = (GHC.complement)

negate :: Usize -> Usize
{-# inline negate #-}
negate = (GHC.negate)

{-addC :: Usize -> Usize -> (Usize,Usize)-}
{-subC :: Usize -> Usize -> (Usize,Usize)-}

gt :: Usize -> Usize -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: Usize -> Usize -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: Usize -> Usize -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: Usize -> Usize -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: Usize -> Usize -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: Usize -> Usize -> Bool
{-# inline ne #-}
ne = (GHC./=)

shift :: Usize -> Isize -> Usize
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: Usize -> Isize -> Usize
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: Usize -> Isize -> Usize
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: Usize -> Isize -> Usize
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: Usize -> Isize -> Usize
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: Usize -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: Usize -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: Usize -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: Usize -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> Usize
{-# inline bit #-}
bit (Isize i) = GHC.bit i
