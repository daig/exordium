module EqF where
import Bool

class EqF f where
  {-# minimal liftEq #-}
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool
  eqC :: f a -> f b -> Bool
  eqC = liftEq (\_ _ -> True)

eq1 :: (EqF f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)
