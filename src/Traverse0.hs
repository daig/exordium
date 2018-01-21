module Traverse0 (module Traverse0, module X) where
import Traverse0.Class as X
{-import FoldMap1 as X-}
import Traverse as X
import K.Type

traverse0_foldMap1 :: (Traverse0 t,Zero m) => (a -> m) -> t a -> m
traverse0_foldMap1 f ta = case traverse0 (\a -> K (f a)) ta of K m -> m
