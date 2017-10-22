module Getter (type (^~), getter, module X) where
import Lens as X
import Comap as X
import Map as X
import Dimap

-- Linear fold
type s ^~ a = forall f. (Map f, Comap f) => (a -> f a) -> s -> f s

-- type is too specific
getter :: (s -> a) -> s ^~ a
getter f = dimap f (comap f)
