{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Coerce
  (type (=#),coerce,coerce#
  ,fromInteger
  ) where
import Types
import qualified Data.Coerce as C
import Unsafe.Coerce
import qualified Prelude as P
import GHC.Classes (Eq(..))

-- | Representational type equality. Contrast with nominal equality `~`
type (=#) = C.Coercible
coerce :: forall b a. a =# b => a -> b
coerce = C.coerce

coerce# :: forall b a. a -> b
coerce# = unsafeCoerce

class NoC a
instance NoC a
-- | A map from a to b which is morally a natural homomorphism, preserving at least the structure indicated by Preserves. See instance definitiions for examples of correct usage
type family AllSatisfied (cs :: [* -> Constraint]) (a :: *) = (c :: Constraint) | c -> cs a where
  AllSatisfied '[] a = NoC a
  AllSatisfied (c ': cs) a = (c a, AllSatisfied cs a)
class (AllSatisfied (Preserves b a) a, AllSatisfied (Preserves b a) b) => Cast# b a where
  type Preserves b a :: [* -> Constraint]
  cast# :: a -> b

fromInteger :: Cast# a Integer => Integer -> a
fromInteger = cast#
fromString :: Cast# a [Char] => [Char] -> a
fromString = cast#

instance Cast# a a where
  type Preserves a a = '[]
  cast# a = a
  

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
