module Hoist where

class Hoist t where hoist :: (forall x. f x -> g x) -> t f a -> t g a
