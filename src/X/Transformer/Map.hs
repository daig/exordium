module X.Transformer.Map (TMap(..), module X) where
import X.Type.NatTrans as X

--  mapt f > mapt g = mapt (f > g)
class TMap t where
  {-# minimal tmap #-}
  tmap :: (f --> g) -> (t f --> t g)
  constTmap :: (forall x. g x) -> t f a -> t g a
  constTmap gx = tmap (\_ -> gx)
