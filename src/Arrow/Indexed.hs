{-# language UndecidableSuperClasses #-}
module Arrow.Indexed (PIndexed(..), ITraversing(..),Traversing(..),module X) where
import Arrow.Postcoerce
import Functor.Coerce1
import Arrow.Closed
import Optic.Traversing
import Arrow.Compose
import Arrow.Promap as X

class (Promap p, PIndexed i q q) => PIndexed i q p | p -> q where
  pix :: p a b -> i -> q a b
  default pix :: q ~ p => p a b -> i -> q a b
  pix p _ = p
  

newtype ITraversing i f a b = ITraversing {runITraversing :: i -> a -> f b}


instance Zip f => Closed (ITraversing i f) where
  closed (ITraversing iafb) = ITraversing (\i xa -> distribute (\x -> iafb i (xa x)))
instance Map f => Promap (ITraversing i f) where promap f g (ITraversing s) = ITraversing (\i -> (promap f (map g) (s i)))
instance Map f => Map (ITraversing i f a) where
  map f (ITraversing s) = ITraversing (\i a -> map f (s i a))
{--- TODO: move to PromapIso class-}
instance Comap f => Comap (ITraversing i f a) where
  comap f (ITraversing s) = ITraversing (\i a -> comap f (s i a))
instance Coerce1 f => Postcoerce (ITraversing i f) where
   postcoerce (ITraversing s) = ITraversing (\i a -> coerce1 (s i a))

instance Map f => Traversed_ (ITraversing i f) where
  traversal_ afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Pure f => Traversed' (ITraversing i f) where
  prism pat constr (ITraversing iafb) = ITraversing (\i s -> case pat s of
    L t -> pure t
    R a -> constr `map` iafb i a)
instance Apply f => Traversed1 (ITraversing i f) where
  traversal1 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Pure f => Traversed0 (ITraversing i f) where
  traversal0 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Applicative f => Traversed (ITraversing i f) where
  traversal afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)

instance PIndexed i (->) (->)
instance Map f => PIndexed i (Traversing f) (Traversing f)
instance (i ~ j, Map f) => PIndexed i (Traversing f) (ITraversing j f) where
  pix (ITraversing f) i = Traversing (f i)
