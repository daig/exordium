module Prism (Prism(..), module X) where
import Dimap  as X
import Sum as X
import Option as X

{-(forall f. Pure f => (a -> f b) -> s -> f t)-}
p seta bt afb s = case seta s of
  L t -> pure t
  R a -> bt `map` afb a


class Dimap p => Prism p where
  {-# minimal prism | left | right #-}
  prism :: (s -> E t a) -> (b -> t) -> p a b -> p s t
  prism pat constr = \p -> dimap pat (either (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism (either R (\x -> L (R x))) L
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap swap swap (left p)

instance Prism (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

type (s +~  a) b t = forall p. Prism p => p a b -> p s t
type  s +~~ a      = forall p. Prism p => p a a -> p s s

prism' :: (s -> (?) a) -> (b -> s) -> (s +~ a) b s
prism' sma = prism (\s -> case sma s of {Some a -> R a; None -> L s})

