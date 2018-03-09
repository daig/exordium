module IForget where
import Indexable.Class
import Optic.View
import K
import E.Utils

newtype IForget i r a b = IForget {runIForget :: i -> a -> r}
{-_IForget :: Promap p => p (a -> r) (a' -> r') -> p (IForget r a b) (IForget r' a' b')-}
{-_IForget = promap runIForget IForget-}
-- _IForget :: Promap p => p (IForget r a b) (IForget r' a' b') -> p (a -> r) (a' -> r')
-- _IForget = promap IForget runIForget
instance Traversed_ (IForget i r) where
  first (IForget iz) = IForget (\i (a,_) -> iz i a)
  traversal_ l (IForget iar) = IForget (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})
instance Promap (IForget i r) where
  promap f _ (IForget iz) = IForget (\i -> colmap f (iz i))
instance ComapL (IForget i r) where colmap f (IForget iz) = IForget (\i -> colmap f (iz i))
instance MapR (IForget i r) where rmap _ (IForget iz) = IForget iz
instance MapIso (IForget i r a) where mapIso = map_mapIso
instance Map (IForget i r a) where map = rmap_map
instance BiComap (IForget i r) where
  bicomap f _ (IForget iz) = IForget (\i -> colmap f (iz i))
instance Comap (IForget i r a) where comap = cormap
instance ComapR (IForget i r) where cormap _ (IForget iz) = IForget iz

instance Zero r => Traversed' (IForget i r) where
  left (IForget iz) = IForget (\i -> e'bifoldMap (iz i) (\_ -> zero))

instance PlusZero r => Traversed (IForget i r) where
  traversal l (IForget iar) = IForget (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})
instance Zero r => Traversed0 (IForget i r) where
  traversal0 l (IForget iar) = IForget (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})
instance Plus r => Traversed1 (IForget i r) where
  traversal1 l (IForget iar) = IForget (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})

instance Cochoice (IForget i r) where
  unleft (IForget ir) = IForget (\i a -> ir i (L a))
  unright (IForget ir) = IForget (\i a -> ir i (R a))

instance (j ~ i) => Indexed j (IForget i r) where
  type Unindexed (IForget i r) = View r
  indexed (IForget iar) i = View (iar i)
instance Indexed i (View r)
