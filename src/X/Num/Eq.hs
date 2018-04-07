module X.Num.Eq (Eq(..),module X) where
import X.Data.Bool as X
import X.Num.Eq' as X

-- | comparable = True
--   eq' a b = Just (eq a b)
class Eq' a => Eq a
