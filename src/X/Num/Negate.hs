{-# language MagicHash #-}
module X.Num.Negate (Negate(..), module X) where
import X.Num.Add0 as X
import GHC.Integer as X
import X.Stock.Ord
import X.Type.Int
import qualified Prelude as P

-- a - a = zero
-- (a - b) - c = a - (b + c)
class Add0 s => Negate s where
  {-# minimal sub | negate #-}
  sub :: s -> s -> s
  infixl 6 `sub`
  a `sub` b = a `add` negate b
  negate :: s -> s
  negate = sub zero
  scalei :: Integer -> s -> s
  scalei n a = case compare# n 0 of
    EQ -> zero
    LT -> scale1# (P.fromInteger (P.abs n)) (negate a)
    GT -> scale1# (P.fromInteger n) a

{-instance Negate Natural where sub n m = m P.- n-}
instance Negate Int where negate = P.negate
