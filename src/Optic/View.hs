module Optic.View (module Optic.View,module X) where
import PlusZero as X
import Traversed as X
import Map as X
import Traversed as X
import Map.Co.Bi as X
import Map.Co as X
import K
import E.Utils
import Map.Pro

newtype View r a b = View {runView :: (a -> r)}
_View :: Promap p => p (View n a a) (View m s s) -> p (a -> n) (s -> m)
_View = promap View runView
view :: (View a a a -> View m s s) -> s -> m
view = (`_View` \x -> x)
instance Traversed_ (View r) where
  first (View z) = View (\(a,_) -> z a)
  traversal_ l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Promap (View r) where
  promap f _ (View z) = View (colmap f z)
instance ComapL (View r) where colmap f (View z) = View (colmap f z)
instance MapR (View r) where rmap _ (View z) = View z
instance MapIso (View r a) where mapIso = map_mapIso
instance Map (View r a) where map = rmap_map
instance BiComap (View r) where
  bicomap f _ (View z) = View (colmap f z)
instance Comap (View r a) where comap = cormap
instance ComapR (View r) where cormap _ (View z) = View z

instance Zero r => Traversed' (View r) where
  right (View z) = View (e'bifoldMap (\_ -> zero) z)

instance PlusZero r => Traversed (View r) where
  traversal l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Zero r => Traversed0 (View r) where
  traversal0 l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Plus r => Traversed1 (View r) where
  traversal1 l (View ar) = View (\s -> case (l (\a -> K (ar a))) s of {K r -> r})

{-instance Cochoice (View r) where-}
  {-unleft (View r) = View (\a -> r (L a))-}
  {-unright (View r) = View (\a -> r (R a))-}
{-type (s ~+ a) b t = forall p. Prism p => p a b -> p s t-}
{-unprismoid :: (forall f. Traverse0 f => (f a -> b) -> f s -> t) -> b -> t-}
{-unprismoid fabfst b = fabfst (\_ -> b) (K ())-}
{-_Just :: (Pure g, Traverse0 f) => (f a -> g b) -> f (Maybe a) -> g (Maybe b)-}
{-_Just = (`prismoid` Just) (\case-}
  {-Just a -> R a-}
  {-Nothing -> L Nothing)-}
