{-# language UndecidableSuperClasses #-}
module Measured.Class (module Measured.Class, module X) where
import Type.Int
import PlusZero.Class as X


-- The measure should preserve all monoidlike structures
class PlusZero (Measure a) => Measured a where
  type Measure a
  measure :: a -> Measure a

instance Measured [a] where
  type Measure [a] = Int
  measure = go 0 where go !n = \case {[] -> n; _:xs -> go (n `plus` 1) xs}

instance (Measured a, Measured b) => Measured (a,b) where
  type Measure (a,b) = (Measure a, Measure b)
  measure (a,b) = (measure a, measure b)

-- TODO: add for other tuple sizes
