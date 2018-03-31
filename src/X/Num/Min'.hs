module X.Num.Min' (Min'(..), module X) where
import X.Data.Maybe as X

-- | Idempotent: min' a a = Just a
class Min' a where
  min' :: a -> a -> Maybe a
  {-default min' :: Min a => a -> a -> Maybe a-}
  {-min' a b = Just (min a b)-}
