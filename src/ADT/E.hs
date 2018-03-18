module ADT.E (module ADT.E, module X) where
import Functor.Pure as X
import Functor.Bimap as X
import Functor.Bitraverse as X
import Functor.Swap as X

import Functor.Traverse as X

data E a b = L ~a | R ~b

{-data a + b = L' ~a | R' ~b-}
-- Waiting on GHC bug --
{-data E a b = L' ~a | R ~b-}
{-pattern L :: forall b a. a -> E a b-}
{-pattern L a = L' a-}
{-{-# COMPLETE L , R #-}-}
