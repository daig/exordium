module RelFold
  (type (!~)
  ,module X) where
import Getter as X
import Traversed1 as X

type s !~ a = forall p. (Traversed1 p, BiComap p) => p a a -> p s a
