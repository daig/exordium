module AnIso.Exchange (Exchange(..), module X) where
import Map as X
import Dimap as X

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Dimap (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (\x -> sa (f x)) (\b -> g (bt b))
  premap f (Exchange sa bt) = Exchange (\x -> sa (f x)) bt
  postmap g (Exchange sa bt) = Exchange sa (\b -> g (bt b))
instance Map (Exchange a b s) where map = postmap
