module Negate (Negate(..), module X) where
import Zero as X

import Recip
import Coerce (coerce)

class Zero a => Negate a where negate :: a -> a

