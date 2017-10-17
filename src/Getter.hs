module Getter (type (^~), type (^~.), getter, view, module X) where
import K as X

type (s ^~  a)  = forall f. IsK f => (a -> f a) -> s -> f s
type (s ^~.  a) = (a -> K a a) -> s -> K a s

getter :: (s -> a) -> s ^~ a
getter f = dimap f (comap f)

view :: s ^~. a -> s -> a
view l s = (\(K a) -> a) (l K s)
