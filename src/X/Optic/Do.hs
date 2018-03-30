module X.Optic.Do (module X.Optic.Do, module X) where
import X.Optic.Do.Internal
import X.Arrow.Mapped as X
import X.Arrow.Folded as X

_Do :: Promap p => p (Do f r a b) (Do g  r' s t) -> p (a -> f r) (s -> g r')
_Do = promap Do runDo

doWith :: (Zero r, Map f) => (Do (FK f x) r a b -> Do (FK f x) r s t) -> (a -> f x) -> s -> f r
doWith l afx = case l (Do (\a -> FK (afx a))) of
  Do sfkx -> \s -> case sfkx s of FK fx -> constMap zero fx

newtype Do f r a b = Do {runDo :: a -> f r}
instance Promap (Do f r) where promap f _ (Do b) = Do (premap f b)
instance Traversed_ (Do f r) where lens sa _ (Do x) = Do (\s -> x (sa s))
instance (Pure f, Zero r) => Traversed' (Do f r) where
  prism seta _ (Do x) = Do (\s -> case seta s of
      L _ -> pure zero
      R a -> x a)
instance (Pure f, Zero r) => Traversed0 (Do f r) where
  lens0 seta _ (Do x) = Do (\s -> case seta s of
      L _ -> pure zero
      R a -> x a)
instance (Apply f, Zero r) => Traversed1 (Do f r) where
  traversed1 (Do ax) = Do (\ta -> constMap zero (traverse1 ax ta))
instance (Applicative f, Zero r) => Traversed (Do f r) where
  traversed (Do ax) = Do (\ta -> constMap zero (traverse ax ta))
instance Folded_ (Do f r) where folding_ sa (Do ax) = Do (\s -> ax (sa s))
instance (Pure f, Zero r) => Folded0 (Do f r) where
  folding0 amsm (Do ax) = Do (\s -> unwrapF (amsm (\a -> WrapF (ax a)) s))
instance (Apply f, Add r, Zero r) => Folded1 (Do f r) where
  folding1 amsm (Do ax) = Do (\s -> unwrapF (amsm (\a -> WrapF (ax a)) s))
  folded1 (Do ax) = Do (\fa -> unwrapF (foldMap1 (\a -> WrapF (ax a)) fa))
instance (Applicative f, Add0 r) => Folded (Do f r) where
  folding amsm (Do ax) = Do (\s -> unwrapF (amsm (\a -> WrapF (ax a)) s))
  folded (Do ax) = Do (\fa -> unwrapF (foldMap (\a -> WrapF (ax a)) fa))
instance (Zip f, Zero r) => Closed (Do f r) where zipped = mapped
instance (Zip f,Zero r) => Mapped (Do f r) where
  mapped (Do afx) = Do (\za -> constMap zero (collect afx za))
