{-# language MagicHash #-}
module X.Num.Recip (Recip(..), module X) where
import X.Num.Mul1 as X
import GHC.Integer
import X.Stock.Ord
import qualified Prelude as P

class Mul1 m => Recip m where
  {-# minimal recip | divide #-}
  recip :: m -> m
  recip = divide one
  divide :: m -> m -> m
  divide m n = m `mul` recip n
  powi :: Integer -> m -> m
  powi n m = case P.compare n 0 of
    EQ -> one 
    LT -> pow1# (P.fromInteger (P.abs n)) (recip m)
    GT -> pow1# (P.fromInteger n) m
