module X.Data.E (module X.Data.E, module X) where
import X.Functor.Pure as X
import X.Functor.Bimap as X
import X.Functor.Bitraverse as X
import X.Functor.Swap as X

import X.Functor.Traverse as X

data E a b = L ~a | R ~b

{-data a + b = L' ~a | R' ~b-}
-- Waiting on GHC bug --
{-data E a b = L' ~a | R ~b-}
{-pattern L :: forall b a. a -> E a b-}
{-pattern L a = L' a-}
{-{-# COMPLETE L , R #-}-}
