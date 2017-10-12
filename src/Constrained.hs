module Constrained where
import Trivial

class Constrained (f :: k -> *) where
  type C f :: k -> Constraint
  type C f = Trivial

