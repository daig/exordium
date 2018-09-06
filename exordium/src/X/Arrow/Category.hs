module X.Arrow.Category (Category(..), module X) where
import X.Arrow.Compose as X
import X.Arrow.Identity as X

-- | identity < p = p < identity = p
class (Compose p, Identity p) => Category p

instance Category (->) 
