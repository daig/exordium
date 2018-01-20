module Zipping where
import Prism as X
import Lens as X
import Closed as X
import Distributive
import Class.Map

newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
instance Closed Zipping where closed (Zipping z) = Zipping (\xa xa' x -> z (xa x) (xa' x))
instance Dimap Zipping where dimap f g (Zipping z) = Zipping (\a a' -> g (z (f a) (f a')))
instance MapIso (Zipping a) where mapIso = map_mapIso
instance Map (Zipping a) where map = postmap
