module Sieve
  (Sieve(..), Rep
  ,module X ) where
import Dimap as X
import Map as X
import I

type family Rep (p :: i -> j -> *) :: j -> *

class (Dimap p, Map (Rep p)) => Sieve p where
  sieve :: p a b -> a -> Rep p b


type instance Rep (->) = I
instance Sieve (->) where
  sieve f a = I (f a)
