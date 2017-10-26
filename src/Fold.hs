module Fold
  (type (.~)
  ,module X) where
import Traversed as X
import BiComap as X
import AffFold as X
import RelFold as X

type s .~ a = forall p. (Traversed p, BiComap p) => p a a -> p s s
