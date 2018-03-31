module X.Num.Max (Max(..), module X) where
import X.Num.Max' as X

-- | Idempotent: max a a = a
--   max' a b = Just (max a b)
class Max' a => Max a where max :: a -> a -> a
