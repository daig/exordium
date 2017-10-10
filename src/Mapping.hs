module Mapping (Mapping(..), module X) where
import Traversing as X
import Closed as X

class (Closed p, Traversing p) => Mapping p where mapping :: Map f => p a b -> p (f a) (f b)
