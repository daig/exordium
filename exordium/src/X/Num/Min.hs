module X.Num.Min (Min(..), module X) where
import X.Num.Min' as X

-- | Idempotent: min a a = a
--   min' a b = Just (min a b)
class Min' a => Min a where min :: a -> a -> a
