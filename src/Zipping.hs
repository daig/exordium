module Zipping where
import Traversed as X
import Closed as X
import Map
import Map.Pro

newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
instance Closed Zipping where closed (Zipping z) = Zipping (\xa xa' x -> z (xa x) (xa' x))
instance Promap Zipping where promap f g (Zipping z) = Zipping (\a a' -> g (z (f a) (f a')))
instance ComapL Zipping where colmap = promap_colmap
instance MapR Zipping where rmap = promap_rmap
instance MapIso (Zipping a) where mapIso = map_mapIso
instance Map (Zipping a) where map = rmap_map

_Zipping :: Promap p => p (Zipping a b) (Zipping s t) -> p (a -> a -> b) (s -> s -> t)
_Zipping = promap Zipping runZipping
zipWithOf :: (Zipping a b -> Zipping s t) -> (a -> a -> b) -> s -> s -> t
zipWithOf l z = case l (Zipping z) of Zipping z' -> z'
