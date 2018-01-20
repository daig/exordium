module Zipping where
import Class.Prism as X
import Class.Lens as X
import Class.Closed as X
import Class.Distributive
import Class.Map
import Utils.Dimap

newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
instance Closed Zipping where closed (Zipping z) = Zipping (\xa xa' x -> z (xa x) (xa' x))
instance Dimap Zipping where dimap f g (Zipping z) = Zipping (\a a' -> g (z (f a) (f a')))
instance CoLMap Zipping where colmap = dimap_colmap
instance RMap Zipping where rmap = dimap_rmap
instance MapIso (Zipping a) where mapIso = map_mapIso
instance Map (Zipping a) where map = rmap_map
