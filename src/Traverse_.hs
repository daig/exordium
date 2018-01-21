module Traverse_ (module Traverse_, module X) where
import Traverse_.Class as X
{-import FoldMap1 as X-}
import Traverse0 as X
import Traverse1 as X
import K.Type

traverse__foldMap_ :: Traverse_ t => (a -> m) -> t a -> m
traverse__foldMap_ f ta = case traverse_ (\a -> K (f a)) ta of K m -> m
