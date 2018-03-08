module Coapplicative.Class (module Coapplicative.Class, module X) where

import FoldMap as X
import Apply.Class as X

-- | fold_ (f |$| a) = fold_ f (fold_ a)
class (FoldMap_ f, Apply f) => Coapplicative f
