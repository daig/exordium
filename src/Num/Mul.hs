{-# language MagicHash #-}
module Num.Mul (module Num.Mul, module X) where
import GHC.Natural as X
import GHC.Integer
import Type.Word
import Type.Int
import Bool
import qualified Prelude as P

class Mul m where
  mul :: m -> m -> m
  -- | Raise to the @n+1@ power
  pow1 :: Natural -> m -> m
  pow1 n = pow1# (n P.+1)

-- | Raise to a non-zero @Natural@ power, this is not checked and will loop on 0.
pow1# :: Mul m => Natural -> m -> m
pow1# y0 x0 = f x0 y0 where
  f x y 
    | P.even y = f (x `mul` x) (y `P.quot` 2)
    | y P.== 1 = x
    | P.otherwise = g (x `mul` x) ((y P.- 1) `P.quot` 2) x
  g x y z 
    | P.even y = g (x `mul` x) (y `P.quot` 2) z
    | y P.== 1 = x `mul` z
    | P.otherwise = g (x `mul` x) ((y P.- 1) `P.quot` 2) (x `mul` z)


instance Mul Natural where mul = (P.*)
instance Mul Integer where mul = (P.*)
instance Mul Int where mul = (P.*)
instance Mul Word where mul = (P.*)
instance Mul () where mul _ _ = ()
instance Mul Bool where mul = (P.&&)
