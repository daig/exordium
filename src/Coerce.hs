{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Coerce where
import qualified Data.Coerce as C
import Unsafe.Coerce
import qualified Prelude as P
import Prelude as X (Num)
import qualified GHC.Exts as P
import GHC.Classes (Eq(..))
import Forall
{-import Map.Class-}
{-import Bimap.Class-}
{-import Dimap.Class-}


-- | Representational type equality. Contrast with nominal equality `~`
type (#=) = C.Coercible

class f a #= f g => CoerceF f g a
instance f a #= f g => CoerceF f g a
type f ##= g = ForallF (CoerceF f g)

{-class p a b #= q a b => CoerceP p q a b-}
{-instance p a b #= q a b => CoerceP p q a b-}
{-type p ###= q = Forall2 (CoerceP p q)-}


coerce :: forall b a. a #= b => a -> b
coerce = C.coerce

coerce# :: forall b a. a -> b
coerce# = unsafeCoerce

{-map# :: forall b a f. (Map f, a #= b) => (a -> b) -> f a -> f b-}
{-map# _ = coerce#-}

{-rmap# :: forall b y p a. (Bimap p, y #= b) => (y -> b) -> p a y -> p a b-}
{-rmap# _ = coerce#-}
{-lmap# :: forall a x p b. (Bimap p, x #= a) => (x -> a) -> p x b -> p a b-}
{-lmap# _ = coerce#-}

{-postmap# :: forall b y p a. (Dimap p, y #= b) => (y -> b) -> p a y -> p a b-}
{-postmap# _ = coerce#-}
{-premap# :: forall a x p b. (Dimap p, a #= x) => (a -> x) -> p x b -> p a b-}
{-premap# _ = coerce#-}

{-{-coerced :: forall s t a b. (s #= a, t #= b) => Iso s t a b-}-}
{-{-coerced l = premap# coerce (postmap (map coerce) l)-}-}

{-instance {-# overlappable #-} Cast# a Integer => P.Num a where fromInteger = cast#-}
{-instance {-# overlappable #-} Cast# a [Char] => P.IsString a where fromString = cast#-}

{-class Cast# b a where-}
  {-type Preserves b a :: [* -> Constraint]-}
  {-cast# :: a -> b-}
{-instance Cast# a a where-}
  {-type Preserves a a = '[]-}
  {-cast# = \a -> a-}
  

{-instance Cast# Int Integer where-}
  {-type Preserves Int Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Int8 Integer where-}
  {-type Preserves Int8 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Int16 Integer where-}
  {-type Preserves Int16 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Int32 Integer where-}
  {-type Preserves Int32 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Int64 Integer where-}
  {-type Preserves Int64 Integer = '[]-}
  {-cast# = P.fromInteger-}

{-instance Cast# Word Integer where-}
  {-type Preserves Word Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Word8 Integer where-}
  {-type Preserves Word8 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Word16 Integer where-}
  {-type Preserves Word16 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Word32 Integer where-}
  {-type Preserves Word32 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Word64 Integer where-}
  {-type Preserves Word64 Integer = '[]-}
  {-cast# = P.fromInteger-}
{-instance Cast# Bool Integer where-}
  {-type Preserves Bool Integer = '[]-}
  {-cast# i = case P.mod i 2 == 0 of-}
    {-0 -> False-}
    {-1 -> True-}
    {-_ -> P.error "impossible! mod 2 outside range 0,1 in cast# @Bool @Integer"  -}
