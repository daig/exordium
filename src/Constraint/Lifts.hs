{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Constraint.Lifts (module Constraint.Lifts, module X) where
import Constraint.Witness as X

type Lifts c = LiftsTo c c
class LiftsTo (c :: k -> Constraint) (c' :: k' -> Constraint) (t :: k -> k') where
  liftW :: c a => W (c' (t a))
  default liftW :: c' (t a) => W (c' (t a))
  liftW = W

class Lifting c c' where
  lifting :: c => W c'
  default lifting :: c' => W c'
  lifting = W
  {-default lifting :: (c,c') => (c' => r) -> r-}
  {-lifting r = r-}
instance (c,c') => Lifting c c'
