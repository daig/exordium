module RelFold
  (type (!~)
  ,module X) where
import Getter as X
import Apply as X

type s !~ a = forall f. (Apply f, Comap f) => (a -> f a) -> s -> f s
