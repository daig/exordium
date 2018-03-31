module X.Num.Ord' (Ord'(..),module X) where
import X.Stock.Ord as X (Ordering(..))
import X.Num.Eq' as X

class Eq' a => Ord' a where
  lte :: a -> a -> Bool
  lte a b = case ord' a b of
    Just GT -> F
    _ -> T
  lt  :: a -> a -> Maybe Bool
  lt a b = lte a b `go` lte b a where
    go F F = Nothing
    go T F = Just T
    go _ _ = Just F
  gt  :: a -> a -> Maybe Bool
  gte :: a -> a -> Bool
  gte a b = gt a b `go` gt b a where
    Just T `go`_ = T
    Just F `go` Just F = T
    _ `go` _ = F
  ord' :: a -> a -> Maybe Ordering
  ord' a b = gte a b `go` gte b a where
    go T T = Just EQ
    go T F = Just GT
    go F T = Just LT
    go F F = Nothing

