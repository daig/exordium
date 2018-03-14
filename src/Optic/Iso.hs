module Optic.Iso (module Optic.Iso, module X) where
import Map.Pro
import Zero
import Closed
import Optic.Prism as X

data Iso a b s t = Iso (s -> a) (b -> t)
instance Promap (Iso a b) where
  promap f g (Iso sa bt) = Iso (premap f sa) (postmap g bt)

repIso :: (forall p. Promap p => p a b -> p s t) -> Iso a b s t
repIso p = p (Iso (\a -> a) (\b -> b))


_1 :: (Zero z, Promap p) => p x a -> p (x,z) (a,z)
_1 = promap (\(a,_) -> a) (\a -> (a,zero))

