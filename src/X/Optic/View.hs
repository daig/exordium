module X.Optic.View (module X.Optic.View,module X) where
import X.Num.Add0 as X
import X.Arrow.Mapped as X
import X.Arrow.Loop as X
import X.Arrow.ITraversed as X
{-import X.Arrow.Folded as X-}
import X.Functor.BiComap as X
{-import X.Functor.Comap as X-}
import X.Type.K
import X.Functor.Bifold

newtype View r a b = View {runView :: (a -> r)}
_View :: Promap p => p (View n a a) (View m s s) -> p (a -> n) (s -> m)
_View = promap View runView
view :: (View a a a -> View m s s) -> s -> m
view = (`_View` \x -> x)
instance Promap (View r) where
  promap f _ (View z) = View (premap f z)
instance Map (View r a) where map = postmap
instance BiComap (View r) where
  bicomap f _ (View z) = View (premap f z)
instance Comap (View r a) where comap = cormap
instance Folded_ (View r) where postcoerce (View ar) = View ar
instance Compose (View r) where precompose (View f) _ = View f

instance Zero r => Traversed' (View r) where
  _R (View z) = View (bifoldMap_ (\_ -> zero) z)
instance Add0 r => Traversed (View r) where
  traversal l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Zero r => Traversed0 (View r) where
  traversal0 l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Add r => Traversed1 (View r) where
  traversal1 l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Traversed_ (View r) where
  _1 (View z) = View (\(a,_) -> z a)
  traversal_ l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})

instance PIndexed i (View r) (View r)
instance Zero r => ITraversed' i (View r) (View r) where
  iprism pat _ (View ar) = View (\s -> case pat s of
    L _ -> zero
    R (_,a) -> ar a) 
instance Add0 r => ITraversed i (View r) (View r) where
  itraversal l (View ar) = View (\s -> case (l (\_ a -> K (ar a))) s of {K r -> r})
instance Zero r => ITraversed0 i (View r) (View r) where
  itraversal0 l (View ar) = View (\s -> case (l (\_ a -> K (ar a))) s of {K r -> r})
instance Add r => ITraversed1 i (View r) (View r) where
  itraversal1 l (View ar) = View (\s -> case (l (\_ a -> K (ar a))) s of {K r -> r})
instance ITraversed_ i (View r) (View r) where
  itraversal_ l (View ar) = View (\s -> case (l (\_ a -> K (ar a))) s of {K r -> r})


instance Loop' (View r) where
  loopRight (View exar) = View (\a -> exar (R a))
  loopLeft (View eaxr) = View (\a -> eaxr (L a))

{-instance Zero r => Closed (View r) where-} -- bad
  {-closed (View ar) = View (\_ -> zero)-}

{-instance Add0 r => Mapped (View r) where-}
  {-setter abst (View ar) = View (\_ -> zero)-}
  

{-instance Loop' (View r) where-}
  {-loopLeft (View r) = View (\a -> r (L a))-}
  {-loopRight (View r) = View (\a -> r (R a))-}
{-type (s ~+ a) b t = forall p. Prism p => p a b -> p s t-}
{-unprismoid :: (forall f. Traverse0 f => (f a -> b) -> f s -> t) -> b -> t-}
{-unprismoid fabfst b = fabfst (\_ -> b) (K ())-}
{-_Just :: (Pure g, Traverse0 f) => (f a -> g b) -> f (Maybe a) -> g (Maybe b)-}
{-_Just = (`prismoid` Just) (\case-}
  {-Just a -> R a-}
  {-Nothing -> L Nothing)-}

instance Add r => Folded1 (View r) where
  folding1 amsm (View ar) = View (\s -> amsm ar s)
  folded1 (View ar) = View (foldMap1 ar)
