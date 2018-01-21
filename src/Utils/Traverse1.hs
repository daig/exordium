module Utils.Traverse1 (module Utils.Traverse1, module X) where
import Traverse1.Class as X
{-import Utils.FoldMap1 as X-}
import Utils.Traverse as X
import Type.K

traverse1_foldMap1 :: (Traverse1 t,Plus m) => (a -> m) -> t a -> m
traverse1_foldMap1 f ta = case traverse1 (\a -> K (f a)) ta of K m -> m
