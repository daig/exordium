module X.Num.Ord (Ord(..), module X) where
import X.Num.Ord' as X
import X.Num.Eq as X

-- | lte a b || gte a b = T
--   ord' a b = Just (ord a b)
class (Ord' a, Eq a) => Ord a where
  ord :: a -> a -> Ordering
