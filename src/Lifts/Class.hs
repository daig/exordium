{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Lifts.Class (module Lifts.Class, module X) where
import Type.Witness as X

type Lifts c = LiftsTo c c
class LiftsTo (c :: k -> Constraint) (c' :: k' -> Constraint) (t :: k -> k') where
  liftW :: c a => W (c' (t a))
  default liftW :: c' (t a) => W (c' (t a))
  liftW = W
