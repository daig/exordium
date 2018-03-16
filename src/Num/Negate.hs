{-# language MagicHash #-}
module Num.Negate (Negate(..), module X) where
import Num.Add0 as X
import GHC.Integer as X
import Num.Ord
import qualified Prelude as P

-- a - a = zero
-- (a - b) - c = a - (b + c)
class Add0 s => Negate s where
  {-# minimal sub | negate #-}
  sub :: s -> s -> s
  a `sub` b = a `add` negate b
  negate :: s -> s
  negate = sub zero
  scalei :: Integer -> s -> s
  scalei n a = case compare n 0 of
    EQ -> zero
    LT -> scale1# (P.fromInteger (P.abs n)) (negate a)
    GT -> scale1# (P.fromInteger n) a

instance Negate Natural where sub n m = m P.- n
