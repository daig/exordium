module NatTrans where

type f ~> g = forall a. f a -> g a
newtype f ~>. g = NatTrans (f ~> g)
type (f |~> g) c = forall a. c a => f a -> g a
