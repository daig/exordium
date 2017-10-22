{-# OPTIONS_GHC -Wno-orphans #-}
module Constrained where
import Trivial
import Witness
import Data.Kind (Constraint)

class Constrained (f :: k -> *) where
  type C f :: k -> Constraint
  type C f = Trivial

instance Constrained ((,) x)
instance Constrained ((->) x)

class HasCon a where
  type Con a :: Constraint
  toDict :: a -> W (Con a)
  unDict :: Con a => a
