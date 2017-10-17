module Cons (Cons(..), Snoc(..), module X) where
import Lens.Type as X (type (+~))

class Cons s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: (s +~ (a,s)) (b,t) t
class Snoc s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: (s +~ (s,a)) (t,b) t
