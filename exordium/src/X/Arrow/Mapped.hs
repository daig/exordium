module X.Arrow.Mapped (Mapped(..), module X) where
import X.Arrow.Mapped.Internal
import X.Arrow.Traversed as X
import X.Arrow.Closed as X
import X.Functor.Tabulate as X
import X.Type.I

-- class (Choice p, Corepresentable p, Comonad (Corep p), Traversable (Corep p), Strong p, Representable p, Monad (Rep p), MonadFix (Rep p), Distributive (Rep p), Costrong p, ArrowLoop p, ArrowApply p, ArrowChoice p, Closed p) => Conjoined p where
-- | Yields index-preserving optics
class (Closed p, Traversed p) => Mapped p where
  {-# minimal mapping | mapped | setter #-}
  setter :: ((a -> b) -> s -> t) -> p a b -> p s t -- aka mapping
  setter f = \p -> promap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  mapping :: (forall f. Tabulate f => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping afbsft = setter (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> promap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect
  conjoined :: (p ~ (->) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
  conjoined _ p = p

instance Mapped (->) where setter l = l

-- TODO: new class with unzipping :: (forall f. Zip f => (a -> f b) -> s -> f t) -> p a b -> p s t
