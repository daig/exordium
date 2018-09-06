{-# language UndecidableSuperClasses #-}
module X.Arrow.IndexedOptics where
import X.Arrow.Folded
import X.Functor.Coerce1
import X.Arrow.Closed
{-import X.Arrow.Representable-}
import X.Arrow.Sieve
import X.Optic.Traversing as X
import X.Kind.Type
import X.Type.Int.I
import X.Functor.IMap
import X.Optic.View

class (Promap p) => IPromap i q p | p -> q, p -> i where
  pix :: p a b -> i -> q a b

newtype Ixp i p a b = Ixp {unIxp :: i -> p a b}
instance Promap p => Promap (Ixp i p) where promap f g (Ixp ipab) = Ixp (map (promap f g) ipab)
instance Traversed_ p => Traversed_ (Ixp i p) where lens sa sbt (Ixp ipab) = Ixp (map (lens sa sbt) ipab)
instance Promap p => IPromap i p (Ixp i p) where pix = unIxp

test :: forall a b. Ixp Int (->) a b -> [a] -> [b]
test = coerce (imap @Int @[] @a @b)

_Ixp :: (Ixp i p a b -> q s t) -> (i -> p a b) -> q s t
_Ixp = premap Ixp 

class IPromap i q p => ITraversed_ i q p | p -> q, p -> i where
{---  {-# minimal lens | traversal_ | traversed_ | _1 | _2 #-}-}
  ilens :: (i -> s -> a) -> (s -> b -> t) -> p a b -> q s t
  {-ilens get set = \p -> promap (\x -> (x,get x)) (\(s,b) -> set s b) (_2 p)-}
  {--- | Lift a linear Van Laarhoven traversal into the profunctor-}
  itraversal_ :: (forall f. Map f => (i -> a -> f b) -> s -> f t) -> p a b -> q s t
  {-traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})-}
  {--- | Pass through the product structure of any linearly traversable container-}
  traversed_ :: ITraverse_ t => p a b -> q (t a) (t b)
  {-traversed_ = traversal_ traverse_-}
  {--- | Pass through the second component of a product-}
  {-_2 :: p a b -> p (x,a) (x,b)-}
  {--- _2 = \p -> promap swap swap (_1 p)-}
  {-_2 = traversed_-}
  {--- | Pass through the first component of a product-}
  {-_1 :: p a b -> p (a,y) (b,y)-}
  {--- _1 = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {---  _1 = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  {-_1 = \p -> promap swap swap (_2 p)-}
