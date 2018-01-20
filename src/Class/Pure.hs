module Class.Pure (module Class.Pure, module X) where
import Class.Map as X
import Class.Lifts as X
import Class.Zero as X
import Type.E as X
import Utils.E
import Type.X
import Type.K

-- http://r6research.livejournal.com/28338.html
-- a Pure f is strong with respect to E

-- | Natural laws:
-- distR < right (map f) = map (right f) < distR
-- distR < left f = map (left f) < distR
-- 
-- Derived Laws:
-- distR < L = pure < L
-- dirtR < R = map R
class Map f => Pure f where
  {-# minimal pure | point | distL | distR #-}
  pure :: a -> f a
  pure a = a `constMap` point
  distL :: E (f a) b -> f (E a b)
  distL = \case {L a -> map L a; R b -> map R (pure b)}
  distR :: E a (f b) -> f (E a b)
  distR e = map e'swap (distL (e'swap e))
  point :: f ()
  point = map (\case {L a' -> a'; R r -> case r of {}}) (distR (L @() @(f X) ()))

pure_zero :: (Pure f, Zero a) => f a
pure_zero = pure zero

instance Pure ((->) x) where pure a = \_ -> a
instance Pure (E x) where pure = R
instance Pure [] where pure a = [a]

instance Zero a => Pure (K a) where pure _ = zero
