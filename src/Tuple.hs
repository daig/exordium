{-# language TypeInType #-}
module Tuple where
import NArgs as X
import Box as X
import I as X

type family Tuple' (n :: Nat) = (t :: NArgs n * *) | t -> n
type instance Tuple' 0 = ()
type instance Tuple' 1 = Box
type instance Tuple' 2 = (,)
type instance Tuple' 3 = (,,)
type instance Tuple' 4 = (,,,)
type instance Tuple' 5 = (,,,,)

data (f :*: g) a b = f a :*: g b
data a :* b = a :* b

type family Tuple (n :: Nat) = (t :: NArgs n * *) | t -> n
type instance Tuple 0 = ()
type instance Tuple 1 = I
type instance Tuple 2 = (:*)
type instance Tuple 3 = Tuple3 
type instance Tuple 4 = Tuple4
type instance Tuple 5 = Tuple5

data Tuple3 a b c = Tuple3 a b c
data Tuple4 a b c d = Tuple4 a b c d
data Tuple5 a b c d e = Tuple5 a b c d e
