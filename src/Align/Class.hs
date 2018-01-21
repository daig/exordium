module Align.Class (module Align.Class, module X) where
import Map.Class as X
import Type.These as X

-- | fa |&| fb = map swap (fb |&| fa) TODO: Is this always useful?
-- f |&| empty = map This f
-- empty |&| g = map That g
class Map f => Align f where
  {-# minimal alignWith | align #-}
  align :: f a -> f b -> f (These a b)
  align = alignWith This That These
  alignWith :: (a -> c) -> (b -> c) -> (a -> b -> c) -> f a -> f b -> f c
  alignWith f g h = \a b -> go `map` align a b where
    go = \case
      This x -> f x
      That y -> g y
      These x y -> h x y
  {-both :: (a -> b -> c) -> -}
