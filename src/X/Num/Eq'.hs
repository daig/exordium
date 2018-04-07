module X.Num.Eq' (Eq'(..),module X) where
import X.Data.Maybe as X
import X.Data.Bool as X

class Eq' a where
  {-# minimal eq' | eq, neq #-}
  eq' :: a -> a -> Maybe Bool
  eq' a b = if comparable a b then Just (eq a b) else Nothing
  eq :: a -> a -> Bool
  eq a b = case eq' a b of
    Just T -> T
    _ -> F
  neq :: a -> a -> Bool
  neq a b = case eq' a b of
    Just F -> T
    _ -> F
  {-default eq' :: Eq a => a -> a -> Maybe Bool-}
  {-eq' a b = Just (eq a b)-}
  comparable :: a -> a -> Bool
  comparable a b = case eq' a b of
    Nothing -> F
    _ -> T
