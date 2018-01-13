module Pure (Pure(..), module X) where
import Map as X
import Sum as X
import Void

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
  pure a = a !@ point
  distL :: E (f a) b -> f (E a b)
  distL = \case {L a -> map L a; R b -> map R (pure b)}
  distR :: E a (f b) -> f (E a b)
  distR e = map swap (distL (swap e))
  point :: f ()
  point = map (\case {L a' -> a'; R r -> case r of {}}) (distR (L @() @(f X) ()))

instance Pure ((->) x) where pure a = \_ -> a
instance Pure (E x) where pure = R
instance Pure [] where pure a = [a]