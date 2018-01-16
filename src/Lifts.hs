{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Lifts (Lifts) where
import Forall
import Witness

class Lifts (c :: k -> Constraint) (t :: k -> k) where
  liftW :: c a => W (c (t a))
  default liftW :: c (t a) => W (c (t a))
  liftW = W
