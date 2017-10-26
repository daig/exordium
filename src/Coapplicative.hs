module Coapplicative (Coapplicative,module X) where

import LinFoldMap as X
import Apply as X

-- | fold_ (f |$| a) = fold_ f (fold_ a)
class (LinFoldMap f, Apply f) => Coapplicative f
