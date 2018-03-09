module E (module E, module X) where
import Pure as X
import Map.Bi as X
import Traverse.Bi as X
import E.Utils

import Traverse as X

data E a b = L ~a | R ~b

instance One a => One (E x a) where one = R one

instance Bimap E where bimap = e'bimap

instance MapR E where rmap = e'map
instance MapIso (E a) where mapIso _ = e'map
instance Map (E a) where map = e'map

instance MapL E where lmap = e'lmap

{-data a + b = L' ~a | R' ~b-}
-- Waiting on GHC bug --
{-data E a b = L' ~a | R ~b-}
{-pattern L :: forall b a. a -> E a b-}
{-pattern L a = L' a-}
{-{-# COMPLETE L , R #-}-}

instance BifoldMap E where bifoldMap = e'bifoldMap
instance BifoldMap0 E where bifoldMap0 = e'bifoldMap
instance BifoldMap1 E where bifoldMap1 = e'bifoldMap
instance BifoldMap_ E where bifoldMap_ = e'bifoldMap
instance Bitraverse E where bitraverse = e'bitraverse
instance Bitraverse0 E where bitraverse0 = e'bitraverse
instance Bitraverse1 E where bitraverse1 = e'bitraverse
instance Bitraverse_ E where bitraverse_ = e'bitraverse
