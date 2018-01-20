module Class.Traverse_ (module Class.Traverse_, module X) where
import Class.Map as X
import Class.FoldMap_ as X
import Class.Apply as X
import Class.Traverse0 as X
import Class.Traverse1 as X
import Utils.Tuple

class (Traverse0 t, Traverse1 t,FoldMap_ t) => Traverse_ t where
  traverse_ :: Map f => (a -> f b) -> t a -> f (t b)
  traverse_ f t = sequence_ (map f t)
  sequence_ :: Map f => t (f a) -> f (t a)
  sequence_ = traverse_ (\x -> x)

{-foldMap_Default :: Traverse_ t => (a -> m) -> t a -> m-}
{-foldMap_Default f t = case traverse_ (\x -> K (f x)) t of {K m -> m}-}

instance Traverse_ ((,) x) where traverse_ = tuple'traverse_ map
