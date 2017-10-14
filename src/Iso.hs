module Iso where
import Lens.Type as X

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (map bt)
{-# inline iso #-}

data Exchange a b s t = Exchange (s -> a) (b -> t)
instance Map (Exchange a b s) where map f (Exchange sa bt) = Exchange sa (\b -> f (bt b))
instance Dimap (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (\x -> sa (f x)) (\b -> g (bt b))
  premap f (Exchange sa bt) = Exchange (\x -> sa (f x)) bt
  postmap g (Exchange sa bt) = Exchange sa (\b -> g (bt b))

