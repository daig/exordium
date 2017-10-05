module Compose where
import Map
import Pure
import Bind
import Bimap
import LRPure
import Apply
import Biapply
import I


newtype O f g a = O (f (g a))
newtype OO f g a = OO (g (f a))
type family Stacked (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  Stacked '[] = I
  Stacked (f ': fs) = O f (Stacked fs)
type family Nested (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  Nested '[] = I
  Nested (f ': fs) = OO f (Nested fs)

class LiftM t where liftm :: Bind f => f a -> t f a
instance Pure t => LiftM (OO t) where liftm m = OO (map pure m)

newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}
data Stream f m r = Step (f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r
newtype Stream' f m r = Stream' (f r (m (Stream' f m r)))
instance (Bimap f, Map m) => Map (Stream' f m) where
  map f = go where go (Stream' frms) = Stream' (bimap f (map go) frms)
instance (LRPure f,Map m) => Pure (Stream' f m) where
  pure r = Stream' (inL r)
instance (Biapply f,Apply m) => Apply (Stream' f m) where
  Stream' f |@| Stream' r = Stream' (biapply (rmap (liftA2 (|@|)) f) r)
-- TODO: liftA2 (|@|) is adhoc. Instead add an instance for O and use that
