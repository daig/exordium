{-# language TypeInType #-}
module Tuple where
import NArgs as X
import Box as X

type family Tuple (n :: Nat) = (t :: NArgs n * *) | t -> n
type instance Tuple 0 = ()
type instance Tuple 1 = Box
type instance Tuple 2 = (,)
type instance Tuple 3 = (,,)
type instance Tuple 4 = (,,,)
type instance Tuple 5 = (,,,,)

data (f :*: g) a b = f a :*: g b

