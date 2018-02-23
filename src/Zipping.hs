module Zipping where
import Prism.Class as X
import Traversed_.Class as X
import Closed.Class as X
import Distributive.Class
import Map.Class
import Dimap

newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
instance Closed Zipping where closed (Zipping z) = Zipping (\xa xa' x -> z (xa x) (xa' x))
instance Dimap Zipping where dimap f g (Zipping z) = Zipping (\a a' -> g (z (f a) (f a')))
instance CoLMap Zipping where colmap = dimap_colmap
instance RMap Zipping where rmap = dimap_rmap
instance MapIso (Zipping a) where mapIso = map_mapIso
instance Map (Zipping a) where map = rmap_map
