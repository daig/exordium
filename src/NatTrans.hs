module NatTrans where

type f --> g = forall a. f a -> g a
type p ---> q = forall a b. p a b -> q a b

newtype f -->. g = NatTrans (f --> g)
newtype p --->. q = NatTrans2 (p ---> q)
