{-# language MagicHash #-}
module X.Type.Flip where
import X.Cast.Coerce.Unsafe
import X.Type.K
{-import X.Arrow.Traversed-}
import X.Kind.Type

newtype Flipped f b a = Flip (f a b)  -- TODO: Find general naming for type permutations

{-instance Promap (Flipped K) where promap _ g (Flip (K b)) = Flip (K (g b))-}
{-instance Traversed' (Flipped K) where prism _ bt (Flip (K b)) = Flip (K (bt b))-}

type family Flip (f :: k -> k' -> Type) :: k' -> k -> Type where
  Flip (Flipped f) = f
  Flip f = Flipped f

flip# :: p a b -> Flip p b a
flip# = coerce#

unflip# :: Flip p b a -> p a b
unflip# = coerce#

{-instance Traversed' (Flip K) where-}
  {-left (Flip (K b)) = Flip (K (L b))-}
  {-right (Flip (K b)) = Flip (K (R b))-}
