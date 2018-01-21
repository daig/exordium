module Dimap.Internal (AnIso(..), module X) where
import Dimap.Class as X

data AnIso a b s t = AnIso (s -> a) (b -> t)

{-instance Dimap (AnIso a b) where-}
  {-dimap f g (AnIso sa bt) = AnIso (\x -> sa (f x)) (\b -> g (bt b))-}
  {-premap f (AnIso sa bt) = AnIso (\x -> sa (f x)) bt-}
  {-postmap g (AnIso sa bt) = AnIso sa (\b -> g (bt b))-}
{-instance Map (AnIso a b s) where map = postmap-}
{-instance MapIso (AnIso a b s) where mapIso _ = postmap-}
