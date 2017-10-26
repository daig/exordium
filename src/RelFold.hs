module RelFold
  (type (!~)
  ,module X) where
import Getter as X
import RelTraversed as X

type s !~ a = forall p. (RelTraversed p, BiComap p) => p a a -> p s a
