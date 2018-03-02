module Traverse_.Class (module Traverse_.Class, module X) where
import Map.Class as X
import FoldMap_.Class as X
import Apply.Class as X
import Traverse0.Class as X
import Traverse1.Class as X

class (Traverse0 t, Traverse1 t,FoldMap_ t) => Traverse_ t where
  traverse_ :: Map f => (a -> f b) -> t a -> f (t b)
  traverse_ f t = sequence_ (map f t)
  sequence_ :: Map f => t (f a) -> f (t a)
  sequence_ = traverse_ (\x -> x)

{-foldMap_Default :: Traverse_ t => (a -> m) -> t a -> m-}
{-foldMap_Default f t = case traverse_ (\x -> K (f x)) t of {K m -> m}-}

instance Traverse_ ((,) x) where traverse_ f (x,a) = (x,) `map` f a
