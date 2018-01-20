module Utils.Traverse0 (module Utils.Traverse0, module X) where
import Class.Traverse0 as X
{-import Utils.FoldMap1 as X-}
import Utils.Traverse as X
import Type.K

traverse0_foldMap1 :: (Traverse0 t,Zero m) => (a -> m) -> t a -> m
traverse0_foldMap1 f ta = case traverse0 (\a -> K (f a)) ta of K m -> m
