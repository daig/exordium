module X.Num.Eq (Eq(..),module X) where
import X.Data.Bool as X
import X.Num.Eq' as X

class Eq' a => Eq a where eq :: a -> a -> Bool
