module Type.NatTrans where

type f --> g = forall a. f a -> g a
type p ---> q = forall a b. p a b -> q a b

type f ~~> g = forall a b. (f a -> f b) -> g a -> g b

-- | Naturally transform an optic caried by p into one carried by q
type p ~~~> q = forall a b s t. (p a b -> p s t) -> q a b -> q s t

-- | Transform an optic represented by l into one carried by p
{-type l ~~~~> p = forall a b s t. l a b s t -> p a b -> p s t-}
{-type l ~~~~> p = forall a b s t. (l a b a b -> l a b s t) -> p a b -> p s t-}


newtype f -->. g = NatTrans (f --> g)
newtype p --->. q = NatTrans2 (p ---> q)
