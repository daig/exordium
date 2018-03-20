module Optic.View (module Optic.View,module X) where
import Num.Add0 as X
import Arrow.Mapped as X
import Arrow.Postcoerce as X
import Functor.BiComap as X
import Functor.Comap as X
import Type.K
import Functor.Bifold

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
instance Postcoerce (View r) where postcoerce (View ar) = View ar

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

instance Cochoice (View r) where
  un_R (View exar) = View (\a -> exar (R a))
  un_L (View eaxr) = View (\a -> eaxr (L a))

{-instance Zero r => Closed (View r) where-} -- bad
  {-closed (View ar) = View (\_ -> zero)-}

{-instance Add0 r => Mapped (View r) where-}
  {-setter abst (View ar) = View (\_ -> zero)-}
  

{-instance Cochoice (View r) where-}
  {-un_L (View r) = View (\a -> r (L a))-}
  {-un_R (View r) = View (\a -> r (R a))-}
{-type (s ~+ a) b t = forall p. Prism p => p a b -> p s t-}
{-unprismoid :: (forall f. Traverse0 f => (f a -> b) -> f s -> t) -> b -> t-}
{-unprismoid fabfst b = fabfst (\_ -> b) (K ())-}
{-_Just :: (Pure g, Traverse0 f) => (f a -> g b) -> f (Maybe a) -> g (Maybe b)-}
{-_Just = (`prismoid` Just) (\case-}
  {-Just a -> R a-}
  {-Nothing -> L Nothing)-}
