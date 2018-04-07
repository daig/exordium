{-# language UndecidableSuperClasses #-}
module X.Arrow.Indexed (module X.Arrow.Indexed,module X) where
import X.Arrow.Folded
import X.Functor.Coerce1
import X.Arrow.Closed
{-import X.Arrow.Representable-}
import X.Arrow.Sieve
import X.Optic.Traversing as X

class (Promap p, PIndexed i q q) => PIndexed i q p | p -> q where
  pix :: p a b -> i -> q a b
  default pix :: q ~ p => p a b -> i -> q a b
  pix p _ = p
  

newtype ITraversing i f a b = ITraversing {runITraversing :: i -> a -> f b}
_ITraversing :: Promap p => p (ITraversing i f a b) (Traversing f s t) -> p (i -> a -> f b) (s -> f t)
_ITraversing = promap ITraversing runTraversing



instance Zip f => Closed (ITraversing i f) where
  closed (ITraversing iafb) = ITraversing (\i xa -> distribute (\x -> iafb i (xa x)))
instance Map f => Promap (ITraversing i f) where promap f g (ITraversing s) = ITraversing (\i -> (promap f (map g) (s i)))
instance Strong f => Strong (ITraversing i f a) where
  strong x (ITraversing iafb) = ITraversing (\i a -> strong x (iafb i a))
instance Map f => Map (ITraversing i f a) where
  map f (ITraversing s) = ITraversing (\i a -> map f (s i a))
instance Remap f => Remap (ITraversing i f a) where
  remap f g (ITraversing s) = ITraversing (\i a -> remap f g (s i a))
{--- TODO: move to PromapIso class-}
instance Comap f => Comap (ITraversing i f a) where
  comap f (ITraversing s) = ITraversing (\i a -> comap f (s i a))
instance Coerce1 f => Folded_ (ITraversing i f) where
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

{-icompose :: (i -> j -> k) -> (ITraversing i f s t -> r) -> (ITraversing j f a b -> Traversing f s t) -> (ITraversing k f a b -> r)-}
{-icompose ijk istr jabst pab = istr (ITraversing (\i s -> jabst (ITraversing (\j -> (runITraversing pab (ijk i j)))) `runTraversing` s))-}

icompose :: (i -> j -> k) -> (ITraversing i f s t -> r) -> (ITraversing j f a b -> Traversing f s t) -> (ITraversing k f a b -> r)
icompose ijk istr jabst pab = istr (ITraversing (\i s -> jabst (ITraversing (\j -> (runITraversing pab (ijk i j)))) `runTraversing` s))

rcompose :: (PIndexed k q p, Sieve q)
         => (i -> j -> k)
         -> (ITraversing i (Rep q) s t -> r)
         -> (ITraversing j (Rep q) a b -> q s t)
         -> (p a b -> r)
rcompose ijk istr jabst pab = istr (ITraversing (\i -> sieve (jabst (ITraversing (\j -> sieve (pix pab (ijk i j)))))))
{-newtype IP i p a b = IP {runIP :: i -> p a b}-}
{-instance Promap p => Promap (IP i p) where promap f g (IP ip) = IP (\i -> promap f g (ip i))-}
{-instance PIndexed i p p => PIndexed i p (IP i p) where pix = runIP-}

{-pcompose :: PIndexed k q p => (i -> j -> k) -> (IP i q s t -> r) -> (IP j q a b -> q s t) -> (p a b -> r)-}
{-pcompose ijk istr jabst pab = istr (IP (\i -> jabst (IP (\j -> pix pab (ijk i j)))))-}
