module X.Num.Bottom (Bottom(..), module X) where
import X.Num.Ord' as X

-- | lte bottom a = T
class Ord' a => Bottom a where bottom :: a
