module Getter (type (^~), module X) where
import Lens as X
import BiComap as X
import Map as X
import Dimap

-- Linear fold
type s ^~ a = forall p. (Dimap p, BiComap p) => p a a -> p s s
