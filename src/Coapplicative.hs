module Coapplicative (Coapplicative,module X) where

import FoldMap_ as X
import Apply as X

-- | fold_ (f |$| a) = fold_ f (fold_ a)
class (FoldMap_ f, Apply f) => Coapplicative f
