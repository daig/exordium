module Implies.Class (module Implies.Class, module X) where
import Type.Witness as X

class Implies (c :: k -> Constraint) (c' :: k -> Constraint) where
  implies :: c a => W (c' a)
  default implies :: c' a => W (c' a)
  implies = W


