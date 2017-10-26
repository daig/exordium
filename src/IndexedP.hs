module IndexedP where
import Witness as X (W(..))
import Traversed as X (Traversed)
import Applicative as X

class IndexedP p where ixmap :: (i -> j) -> p j a b -> p i a b

class IndexedP p => ITraversed p where
  {-# minimal itraversing | iwander #-}
  itraversing :: p (i,o) a b -> p o (t a) (t b)
  iwander :: (forall f. Applicative f => (i -> a -> f b) -> (s -> f t)) -> p (i,o) a b -> p o s t

  traversingIWitness :: W (Traversed (p i))
  default traversingIWitness :: Traversed (p i) => W (Traversed (p i))
  traversingIWitness = W

newtype StarI f i a b = StarI {runStarI :: i -> a -> f b}
newtype ForgetI r i a b = ForgetI {runForgetI :: a -> r}
newtype IForget r i a b = IForget {runIForget :: i -> a -> r}

type AFoldI' s a i o r = ForgetI r i a a -> ForgetI r o s s
foldMapOfI :: AFoldI' s a i o r -> (a -> r) -> s -> r
foldMapOfI o f = runForgetI (o (ForgetI f))

ifoldMapOf :: (IForget r i a a -> IForget r () s s) -> (i -> a -> r) -> s -> r
ifoldMapOf o f = runIForget (o (IForget f)) ()
