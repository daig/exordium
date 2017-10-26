module MapT where

class MapT t where
  {-# minimal mapt #-}
  mapt :: (forall x. f x -> g x) -> t f a -> t g a
  constMapt :: (forall x. g x) -> t f a -> t g a
  constMapt gx = mapt (\_ -> gx)
