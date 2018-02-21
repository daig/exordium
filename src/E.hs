module E where
import Pure.Class as X
import Bimap.Class as X

import Traverse0.Class as X

data E a b = L ~a | R ~b

instance Pure (E x) where pure = R
e'bifoldMap_ f g = \case
  L a -> f a
  R b -> g b

e'swap = e'bifoldMap_ R L

instance Bimap E where bimap = e'bimap
e'bimap f g = e'bifoldMap_ (\a -> L (f a)) (\b -> R (g b))

instance RMap E where rmap = e'map
instance MapIso (E a) where mapIso _ = e'map
instance Map (E a) where map = e'map
e'map = e'bimap (\a -> a)

instance LMap E where lmap = e'lmap
e'lmap = (`e'bimap` (\b -> b))

{-data a + b = L' ~a | R' ~b-}
-- Waiting on GHC bug --
{-data E a b = L' ~a | R ~b-}
{-pattern L :: forall b a. a -> E a b-}
{-pattern L a = L' a-}
{-{-# COMPLETE L , R #-}-}

instance Traverse0 (E x) where
  traverse0 afb = \case
    L x -> pure (L x)
    R a -> map R (afb a)
instance Traverse (E x) where traverse = traverse0
instance FoldMap0 (E x)
instance FoldMap (E x)
