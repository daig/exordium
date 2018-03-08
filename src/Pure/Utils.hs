module Pure.Utils where
import X
import Pure.Class
import Traverse.Bi
import E

distL :: (Bitraverse_ t, Pure f) => t (f a) b -> f (t a b)
distL = bitraverse_ (\fa -> fa) pure

distR :: (Bitraverse_ t, Pure f) => t a (f b) -> f (t a b)
distR = bitraverse_ pure (\fb -> fb)
{-distR e = map e'swap (distL (e'swap e)) where e'swap = \case {L a -> R a; R b -> L b}-}
-- | point = map (\case {L a' -> a'; R r -> case r of {}}) (distR (L @() @(f X) ()))
point :: forall f. Pure f => f ()
point = pure ()

pure_zero :: (Pure f, Zero a) => f a
pure_zero = pure zero

point_pure :: Pure f => b -> f b
point_pure a = a `constMap` point
