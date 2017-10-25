module Coapplicative (Coapplicative,module X) where

import Extract as X
import Apply as X

-- | extract (f |$| a) = extract f (extract a)
class (LinFoldMap f, Apply f) => Coapplicative f
