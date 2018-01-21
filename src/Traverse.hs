module Traverse (module Traverse, module X) where
import Traverse.Class as X
import I.Type
import K.Type

traverse_map :: Traverse t => (a -> b) -> t a -> t b
traverse_map f ta = case traverse (\a -> I (f a)) ta of I tb -> tb

traverse_foldMap :: (Traverse t,PlusZero m) => (a -> m) -> t a -> m
traverse_foldMap f ta = case traverse (\a -> K (f a)) ta of K m -> m
