{-# language MagicHash #-}
module Dimap where

class Dimap p where
  {-# minimal dimap | premap,postmap #-}
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g h = postmap g (premap f h)
  premap :: (a -> x) -> p x b -> p a b
  premap f = dimap f (\x -> x)
  constpremap :: x -> p x b -> p a b
  constpremap x = premap (\_ -> x)
  postmap :: (y -> b) -> p a y -> p a b
  postmap = dimap (\x -> x)
  constpostmap :: b -> p a y -> p a b
  constpostmap b = postmap (\_ -> b)

(!<) :: Dimap p => b -> p a y -> p a b
(!<) = constpostmap
(>!) :: Dimap p => p a y -> b -> p a b
p >! b = constpostmap b p
(!>@) :: Dimap p => x -> p x b -> p a b
(!>@) = constpremap
(@<!) :: Dimap p => p x b -> x -> p a b
p @<! x = constpremap x p

(<@) :: Dimap p => (y -> b) -> p a y -> p a b
(<@) = postmap
(>@) :: Dimap p => (a -> x) -> p x b -> p a b
(>@) = premap
(@>) :: Dimap p => p x b -> (a -> x) -> p a b
p @> f = premap f p
(>@>) :: Dimap p => (a -> x) -> (y -> b) -> p x y -> p a b
(>@>) = dimap
(>@|) :: Dimap p => (a -> x) -> p x y -> (y -> b) -> p a b
(f >@| p) g = dimap f g p
(|@>) :: ((y -> b) -> p a b) -> (y -> b) -> p a b
k |@> f = k f
(<@|) :: Dimap p => (y -> b) -> p x y -> (a -> x) -> p a b
(g <@| p) f = dimap f g p
(|@<) :: ((a -> x) -> p a b) -> (a -> x) -> p a b
k |@< f = k f
instance Dimap (->) where dimap f g h a = g (h (f a))
