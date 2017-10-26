module Functor (Functor(..), module X) where
import Category as X
import GHC.Types (Constraint)

-- A functor from "Arr f" into Hask
-- fmap id = id
-- fmap (f < g) = fmap f < fmap g
class Category (Arr f) => Functor (f :: k -> *) where
  type Arr f :: k -> k -> *
  type C f :: k -> Constraint
  fmap :: (C f a, C f b) => Arr f a b -> f a -> f b
