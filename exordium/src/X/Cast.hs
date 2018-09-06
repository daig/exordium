{-# language UndecidableSuperClasses #-}
{-# language MagicHash #-}
module X.Cast where
import X.Type.Word
{-import X.Kind.Nat-}
{-import Prelude (Enum(..),(>),(<),(||),fromInteger,minBound,maxBound,Bounded)-}
import X.Type.Int.I
{-import X.Type.Word.W8-}
import X.Optic.Prism
{-import X.Optic.Re (re)-}

import X.Cast.Internal

class super ~>~ sub where
  {-# minimal _cast | upcast, downcast' #-}
  _cast :: Traversed' p => p sub sub -> p super super
  _cast = prism' downcast' upcast
  upcast :: sub -> super
  upcast = review _cast
  downcast' :: super -> Maybe sub
  downcast' = _View _cast Just

_cast__cast :: (a ~=~ b, Promap p) => p a a -> p b b
_cast__cast = _cast_

instance a ~>~ a where _cast = _cast__cast

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

instance Int ~>~ Word8 where _cast = _enum
instance Int ~>~ Word16 where _cast = _enum
instance Int ~>~ Word32 where _cast = _enum
{-instance Word64 ~>~ Int where _cast = _enum-}
{-instance Word ~>~ Int where _cast = _enum-}
  {-_cast = prism down up where-}
    {-down i = if i > 255 || i < 0 then L i else R (toEnum i)-}
    {-up = fromEnum-}
