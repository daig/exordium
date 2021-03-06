module X.Constraint.Implies (Implies(..), module X) where
import X.Constraint.Witness as X

class Implies (c :: k -> Constraint) (c' :: k -> Constraint) where
  implies :: c a => W (c' a)
  default implies :: c' a => W (c' a)
  implies = W


