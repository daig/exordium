module Utils.Traverse_ (module Utils.Traverse_, module X) where
import Class.Traverse_ as X
{-import Utils.FoldMap1 as X-}
import Utils.Traverse0 as X
import Utils.Traverse1 as X
import Type.K

traverse__foldMap_ :: Traverse_ t => (a -> m) -> t a -> m
traverse__foldMap_ f ta = case traverse_ (\a -> K (f a)) ta of K m -> m
