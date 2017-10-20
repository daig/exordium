module NatTrans where

type f ~> g = forall a. f a -> g a
newtype f ~>. g = NatTrans (f ~> g)
type (f |~> g) c = forall a. c a => f a -> g a

type p :~> q = forall a b. p a b -> q a b
newtype p :~>. q = NatTrans2 (p :~> q)
