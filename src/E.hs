module E (module E, module X) where
import Pure as X
import Map.Bi as X
import Traverse.Bi as X
import Swap as X

import Traverse as X

data E a b = L ~a | R ~b

{-data a + b = L' ~a | R' ~b-}
-- Waiting on GHC bug --
{-data E a b = L' ~a | R ~b-}
{-pattern L :: forall b a. a -> E a b-}
{-pattern L a = L' a-}
{-{-# COMPLETE L , R #-}-}
