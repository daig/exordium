module MapT (MapT(..), module X) where
import NatTrans as X

--  mapt f > mapt g = mapt (f > g)
class MapT t where
  {-# minimal mapT #-}
  mapT :: (f --> g) -> (t f --> t g)
  constMapt :: (forall x. g x) -> t f a -> t g a
  constMapt gx = mapT (\_ -> gx)
