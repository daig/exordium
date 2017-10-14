{-# OPTIONS_GHC -Wno-orphans #-}
module Constrained where
import Trivial

class Constrained (f :: k -> *) where
  type C f :: k -> Constraint
  type C f = Trivial

instance Constrained ((,) x)
instance Constrained ((->) x)
