module Mapping.Internal (Bar(..), module X) where
import Applicative.Class as X
import Distributive.Class as X
import Map

newtype Bar t b a = Bar {runBar :: forall f. (Applicative f, Distributive f) => (a -> f b) -> f t}
instance MapIso (Bar t b) where mapIso = map_mapIso
instance Map (Bar t b) where map f (Bar k) = Bar (\xfb -> k (\x -> xfb (f x)))
