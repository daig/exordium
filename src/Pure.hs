module Pure where
import Map
import Sum
import Void

-- http://r6research.livejournal.com/28338.html

class Map f => Pure f where
  {-# minimal pure | point | distL | distR #-}
  pure :: a -> f a
  pure a = constMap a point
  distL :: E (f a) b -> f (E a b)
  distL = \case {L a -> map L a; R b -> map R (pure b)}
  distR :: E a (f b) -> f (E a b)
  distR e = map swap (distL (swap e))
  point :: f ()
  point = map (\case {L a' -> a'; R r -> case r of {}}) (distR (L @() @(f X) ()))
