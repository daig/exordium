module Mapping.Bar (Bar(..), module X) where
import Applicative as X
import Distributive as X

newtype Bar t b a = Bar {runBar :: forall f. (Applicative f, Distributive f) => (a -> f b) -> f t}
instance Map (Bar t b) where map f (Bar k) = Bar (\xfb -> k (\x -> xfb (f x)))
