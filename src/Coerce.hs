{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Coerce
  (type (#=),coerce,coerce#,map#,lmap#,rmap#,premap#,postmap#
  ,fromInteger, fromString, ifThenElse
  ) where
import Types
import qualified Data.Coerce as C
import Unsafe.Coerce
import qualified Prelude as P
import GHC.Classes (Eq(..))
import Map
import Bimap
import Dimap
import Trivial

-- | Representational type equality. Contrast with nominal equality `~`
type (#=) = C.Coercible
coerce :: forall b a. a #= b => a -> b
coerce = C.coerce

coerce# :: forall b a. a -> b
coerce# = unsafeCoerce

map# :: forall b a f. (Map f, a #= b) => (a -> b) -> f a -> f b
map# _ = coerce#

rmap# :: forall b y p a. (Bimap p, y #= b) => (y -> b) -> p a y -> p a b
rmap# _ = coerce#
lmap# :: forall a x p b. (Bimap p, x #= a) => (x -> a) -> p x b -> p a b
lmap# _ = coerce#

postmap# :: forall b y p a. (Dimap p, y #= b) => (y -> b) -> p a y -> p a b
postmap# _ = coerce#
premap# :: forall a x p b. (Dimap p, a #= x) => (a -> x) -> p x b -> p a b
premap# _ = coerce#

{-coerced :: forall s t a b. (s #= a, t #= b) => Iso s t a b-}
{-coerced l = premap# coerce (postmap (map coerce) l)-}

fromInteger :: Cast# a Integer => Integer -> a
fromInteger = cast#
fromString :: Cast# a [Char] => [Char] -> a
fromString = cast#
ifThenElse :: Cast# Bool b => b -> a -> a -> a
ifThenElse b t f = case cast# b of
  True -> t
  False -> f

class Cast# b a where
  type Preserves b a :: [* -> Constraint]
  cast# :: a -> b
instance Cast# a a where
  type Preserves a a = '[]
  cast# = \a -> a
  

instance Cast# Int Integer where
  type Preserves Int Integer = '[]
  cast# = P.fromInteger
instance Cast# Int8 Integer where
  type Preserves Int8 Integer = '[]
  cast# = P.fromInteger
instance Cast# Int16 Integer where
  type Preserves Int16 Integer = '[]
  cast# = P.fromInteger
instance Cast# Int32 Integer where
  type Preserves Int32 Integer = '[]
  cast# = P.fromInteger
instance Cast# Int64 Integer where
  type Preserves Int64 Integer = '[]
  cast# = P.fromInteger

instance Cast# Word Integer where
  type Preserves Word Integer = '[]
  cast# = P.fromInteger
instance Cast# Word8 Integer where
  type Preserves Word8 Integer = '[]
  cast# = P.fromInteger
instance Cast# Word16 Integer where
  type Preserves Word16 Integer = '[]
  cast# = P.fromInteger
instance Cast# Word32 Integer where
  type Preserves Word32 Integer = '[]
  cast# = P.fromInteger
instance Cast# Word64 Integer where
  type Preserves Word64 Integer = '[]
  cast# = P.fromInteger
instance Cast# Bool Integer where
  type Preserves Bool Integer = '[]
  cast# i = case P.mod i 2 == 0 of
    0 -> False
    1 -> True
    _ -> P.error "impossible! mod 2 outside range 0,1 in cast# @Bool @Integer"  
