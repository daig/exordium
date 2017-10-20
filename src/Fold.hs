module Fold
  (type (^~)
  ,module X) where
import Applicative as X
import Comap as X
import AffFold as X
import RelFold as X

type s ^~ a = forall f. (Applicative f, Comap f) => (a -> f a) -> s -> f s
