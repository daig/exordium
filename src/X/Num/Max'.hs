module X.Num.Max' (Max'(..), module X) where
import X.Data.Maybe as X

-- | Idempotent: max' a a = Just a
class Max' a where
  max' :: a -> a -> Maybe a
  {-default max' :: Max a => a -> a -> Maybe a-}
  {-max' a b = Just (max a b)-}
