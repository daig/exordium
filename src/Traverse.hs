module Traverse (module Traverse, module X) where
import Traverse.Class as X
import I.Type
import K.Type
import Instances

defaulting 'map [|\f ta -> case traverse (\a -> I (f a)) ta of I tb -> tb|]

defaulting 'foldMap [|\f ta -> case traverse (\a -> K (f a)) ta of K m -> m|]
