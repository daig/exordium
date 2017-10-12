module Cons (Cons(..), Snoc(..), module X) where
import Constrained as X

class Constrained f => Cons f where
  (+|) :: C f a => a -> f a -> f a
class Constrained f => Snoc f where
  (|+) :: C f a => f a -> a -> f a 
-- | Cons a (reverse b) = Snoc b a
-- reverse . reverse = id
class Reverse f where reverse :: f a -> f a
