module Cons where

class Cons f g where (+|) :: f a -> g a -> g a
class Snoc f g where (|+) :: f a -> g a -> f a 
-- | Cons a (reverse b) = Snoc b a
-- reverse . reverse = id
class Reverse f where reverse :: f a -> f a
