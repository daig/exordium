{-# language MagicHash #-}
module Flip where
import Coerce

newtype Flipped f b a = Flip (f a b) 
type family Flip (f :: k -> k' -> *) :: k' -> k -> * where
  Flip (Flipped f) = f
  Flip f = Flipped f

flip# :: p a b -> Flip p b a
flip# = coerce#

unflip# :: Flip p b a -> p a b
unflip# = coerce#
