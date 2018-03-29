module X.Optic.Do (module X.Optic.Do, module X) where
import X.Optic.Do.Internal
import X.Arrow.Mapped as X
import X.Arrow.Folded as X
import X.Type.K as X

_Do :: Promap p => p (Do f a b) (Do g s t) -> p (a -> f ()) (s -> g ())
_Do = promap Do runDo

{-doWith :: Map f => (Do (FK f x) a b -> Do (FK f x) s t) -> (a -> f x) -> s -> f ()-}
{-doWith l afx = case l (Do (\a -> FK (afx a))) of-}
  {-Do sfkx -> \s -> case sfkx s of FK fx -> constMap () fx-}

newtype Do f a b = Do {runDo :: a -> f ()}
instance Promap (Do f) where promap f _ (Do b) = Do (premap f b)
instance Traversed_ (Do f) where lens sa _ (Do x) = Do (\s -> x (sa s))
instance Pure f => Traversed' (Do f) where
  prism seta _ (Do x) = Do (\s -> case seta s of
      L _ -> pure ()
      R a -> x a)
instance Pure f => Traversed0 (Do f) where
  lens0 seta _ (Do x) = Do (\s -> case seta s of
      L _ -> pure ()
      R a -> x a)
instance Apply f => Traversed1 (Do f) where
  traversed1 (Do ax) = Do (\ta -> constMap () (traverse1 ax ta))
instance Applicative f => Traversed (Do f) where
  traversed (Do ax) = Do (\ta -> constMap () (traverse ax ta))
instance Folded_ (Do f) where folding_ sa (Do ax) = Do (\s -> ax (sa s))
instance (Pure f, Zero (f ())) => Folded0 (Do f) where folding0 amsm (Do ax) = Do (\s -> amsm ax s)
instance (Apply f, Add (f ())) => Folded1 (Do f) where
  folding1 amsm (Do ax) = Do (\s -> amsm ax s)
  folded1 (Do ax) = Do (\fa -> foldMap1 ax fa)
instance Zip f => Closed (Do f) where zipped = mapped
instance Zip f => Mapped (Do f) where
  mapped (Do afx) = Do (\za -> constMap () (collect afx za))
