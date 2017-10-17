{-# language UndecidableSuperClasses #-}
module Enriched where

class (Hom a a, Act a a) => Monoid a where
  unit :: a
  plus :: a -> a -> a
  
-- hom a a = zero
-- hom a b + hom b c = hom a c
class Monoid a => Hom v a where hom :: a -> a -> v
-- act zero = id
-- act (a + b) = act a . act b
class Monoid a => Act v a where act :: v -> a -> a

data Boo = F | T
instance Monoid Boo where
  unit = F
  plus F = \x -> x
  plus T = \_ -> T
instance Act Boo Boo where act = plus
