module X.Num.Eq' (Eq'(..),module X) where
import X.Data.Maybe as X
import X.Data.Bool as X

class Eq' a where
  eq' :: a -> a -> Maybe Bool
  {-default eq' :: Eq a => a -> a -> Maybe Bool-}
  {-eq' a b = Just (eq a b)-}
  comparable :: a -> a -> Bool
