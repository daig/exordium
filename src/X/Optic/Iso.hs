{-# language MagicHash #-}
module X.Optic.Iso (module X.Optic.Iso, module X) where
import X.Arrow.Promap
import X.Num.Zero
import X.Arrow.Closed
import X.Optic.Prism as X

data Iso a b s t = Iso (s -> a) (b -> t)
instance Promap (Iso a b) where
  promap f g (Iso sa bt) = Iso (premap f sa) (postmap g bt)

repIso :: (forall p. Promap p => p a b -> p s t) -> Iso a b s t
repIso p = p (Iso (\a -> a) (\b -> b))

