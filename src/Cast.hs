{-# language UndecidableSuperClasses #-}
{-# language MagicHash #-}
module Cast where
import Type.Word
import Kind.Nat
import Prelude (Enum(..),(>),(<),(||),fromInteger,minBound,maxBound,Bounded)
import Type.Int.I
import Type.Word.W8
import Optic.Prism (prism',match,match',preview,view',_View)
import Maybe
import Optic.View (view)
import Optic.Review (review)
import Optic.Re (re)
import Arrow.Traversed (Promap(..),Traversed'(..),E(..),Cochoice)

import qualified Prelude as P
import Data.Coerce (Coercible)
import qualified Data.Coerce as C

import Cast.Internal


-- | Structural equality, which may not preserve semantics of nominal typeclass instances
type (#=#) = Coercible
coerce :: forall b a. a #=# b => a -> b
coerce = C.coerce

_coerce_ :: forall a s p. (Promap p, s #=# a, a #=# s ) => p a a -> p s s
_coerce_ = promap coerce coerce

class a ~>~ b where
  _cast :: Traversed' p => p a a -> p b b
  default _cast :: (a ~=~ b, Traversed' p) => p a a -> p b b
  _cast = _cast_
  {-upcast :: a -> b-}
  {-upcast = review _cast-}
  {-downcast' :: b -> Maybe a-}
  {-downcast' = _View _cast Just-}

-- TODO: annoying, should be part of ~>~ class, but the type application order is wrong
upcast :: forall b a. a ~>~ b => a -> b
upcast = review _cast
downcast' :: a ~>~ b => b -> Maybe a
downcast' = _View _cast Just

instance a ~>~ a where _cast = _cast_

-- TODO: update with quantified constraints when available
-- | Semantic equality, which should preserve all typeclass instances shared between them.
-- law: _cast = _cast_ OR downcast' = Just < downcast
class (a ~>~ b, b ~=~ a) => a ~=~ b where
  {-# minimal _cast_ | downcast #-}
  _cast_ :: Promap p => p a a -> p b b
  _cast_ = promap downcast upcast
  downcast :: b -> a
  downcast = view _cast_
instance a ~=~ a where _cast_ p = p

instance Word8 ~>~ Int where _cast = _enum
instance Word16 ~>~ Int where _cast = _enum
instance Word32 ~>~ Int where _cast = _enum
{-instance Word64 ~>~ Int where _cast = _enum-}
{-instance Word ~>~ Int where _cast = _enum-}
  {-_cast = prism down up where-}
    {-down i = if i > 255 || i < 0 then L i else R (toEnum i)-}
    {-up = fromEnum-}
