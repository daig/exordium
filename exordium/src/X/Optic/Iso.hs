{-# language MagicHash #-}
module X.Optic.Iso (module X.Optic.Iso, module X) where
import X.Arrow.Promap as X
import X.Num.Zero
import X.Arrow.Closed
{-import X.Optic.Prism as X-}
import X.Cast.Coerce
import X.Ops.Fun

data Iso a b s t = Iso (s -> a) (b -> t)
instance Map (Iso a b s) where map tt' (Iso sa bt) = Iso sa (bt > tt')
instance Strong (Iso a b s) where strong = map_strong
instance Remap (Iso a b s) where remap _ = map
instance Comap (BA (Iso a b) t) where comap s's (BA (Iso sa bt)) = BA (Iso (s's > sa) bt)
instance Promap (Iso a b) where
  promap f g (Iso sa bt) = Iso (premap f sa) (postmap g bt)

repIso :: (forall p. Promap p => p a b -> p s t) -> Iso a b s t
repIso p = p (Iso (\a -> a) (\b -> b))

_coerce_ :: (Promap p, s #=# a, b #=# t ) => p a b -> p s t
_coerce_ = promap# coerce coerce

-- TODO: find better name
_coerceF_ :: (g a #=# f a, Promap p) => p (f a) (f a) -> p (g a) (g a)
_coerceF_ = _coerce_

_wrapped_ :: (Promap p, f a #=# a) => p (f a) (f a) -> p a a
_wrapped_ = _coerce_

