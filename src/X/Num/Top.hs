module X.Num.Top (Top(..),module X) where
import X.Num.Ord' as X

-- | gte top a = T
class Ord' a => Top a where top :: a
