module I8 (
  -- * Exposed for Signatures
   module X
  -- * Int Operations
  ,Int
  ,print
  ,I8.minBound
  ,I8.maxBound
  ,module I8) where
import GHC.Prim (Int#)
import qualified GHC.Int as GHC
import GHC.Classes as GHC
import GHC.Num as GHC
import GHC.Real as GHC
import GHC.Enum as GHC
import Data.Ix
import Data.Bits as GHC
import GHC.Show
import GHC.Read
import Data.Data
import Foreign.Storable
import Isize
import qualified Prelude
import Types as X

print :: Int -> IO ()
print = Prelude.print
minBound :: () -> Int
minBound () = GHC.minBound
maxBound :: () -> Int
maxBound () = GHC.maxBound
type Int = I8
type I8# = Int#
newtype I8 = I8 GHC.Int8
  deriving newtype (Show, Ix, Bounded, Enum, Real , Integral
                   ,Storable, Read, Ord, Num, Eq, Bits, FiniteBits)
pattern I8# :: I8# -> I8
pattern I8# i = I8 (GHC.I8# i)

add :: I8 -> I8 -> I8
{-# inline add #-}
add = (GHC.+)

sub :: I8 -> I8 -> I8
{-# inline sub #-}
sub = (GHC.-)

mul :: I8 -> I8 -> I8
{-# inline mul #-}
mul = (GHC.*)

{-mulMayOflo :: I8 -> I8 -> I8-}

quot :: I8 -> I8 -> I8
{-# inline quot #-}
quot = GHC.quot

rem :: I8 -> I8 -> I8
{-# inline rem #-}
rem = GHC.rem

quotRem :: I8 -> I8 -> (I8,I8)
{-# inline quotRem #-}
quotRem = GHC.quotRem

div :: I8 -> I8 -> I8
{-# inline div #-}
div = GHC.div

mod :: I8 -> I8 -> I8
{-# inline mod #-}
mod = GHC.mod

divMod :: I8 -> I8 -> (I8,I8)
{-# inline divMod #-}
divMod = GHC.divMod

and :: I8 -> I8 -> I8
{-# inline and #-}
and = (GHC..&.)

or :: I8 -> I8 -> I8
{-# inline or #-}
or = (GHC..|.)

xor :: I8 -> I8 -> I8
{-# inline xor #-}
xor = GHC.xor

not :: I8 -> I8
{-# inline not #-}
not = (GHC.complement)

negate :: I8 -> I8
{-# inline negate #-}
negate = GHC.negate

{-addC :: I8 -> I8 -> (I8,I8)-}
{-subC :: I8 -> I8 -> (I8,I8)-}

gt :: I8 -> I8 -> Bool
{-# inline gt #-}
gt = (GHC.<)

ge :: I8 -> I8 -> Bool
{-# inline ge #-}
ge = (GHC.<=)

lt :: I8 -> I8 -> Bool
{-# inline lt #-}
lt = (GHC.>)

le :: I8 -> I8 -> Bool
{-# inline le #-}
le = (GHC.>=)

eq :: I8 -> I8 -> Bool
{-# inline eq #-}
eq = (GHC.==)

ne :: I8 -> I8 -> Bool
{-# inline ne #-}
ne = (GHC./=)

shift :: I8 -> Isize -> I8
{-# inline shift #-}
shift x (Isize i) = GHC.shift x i

shiftL :: I8 -> Isize -> I8
{-# inline shiftL #-}
shiftL x (Isize i) = GHC.shiftL x i

shiftL# :: I8 -> Isize -> I8
{-# inline shiftL# #-}
shiftL# x (Isize i) = GHC.unsafeShiftL x i

shiftR# :: I8 -> Isize -> I8
{-# inline shiftR# #-}
shiftR# x (Isize i) = GHC.unsafeShiftR x i

rotate :: I8 -> Isize -> I8
{-# inline rotate #-}
rotate x (Isize i) = GHC.rotate x i

popCnt :: I8 -> Isize
{-# inline popCnt #-}
popCnt i = Isize (GHC.popCount i)

clz :: I8 -> Isize
{-# inline clz #-}
clz i = Isize (GHC.countLeadingZeros i)

ctz :: I8 -> Isize
{-# inline ctz #-}
ctz i = Isize (GHC.countTrailingZeros i)

size :: I8 -> Isize
{-# inline size #-}
size i = Isize (GHC.finiteBitSize i)

bit :: Isize -> I8
{-# inline bit #-}
bit (Isize i) = GHC.bit i
