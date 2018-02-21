module O (module O, module X) where
import {-# source #-} I as X
import Applicative.Class as X

newtype O f g a = O (f (g a))

type family O_ (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  O_ '[] = I
  O_ (f ': fs) = O f (O_ fs)

instance (Pure f,Pure g) => Pure (O f g) where pure a = O (pure (pure a))
instance (Apply f,Apply g) => Apply (O f g) where O fgf `ap`O fga = O (map ap fgf `ap` fga)
instance (Applicative f, Applicative g) => Applicative (O f g)
instance (Map f,Map g) => Map (O f g) where map f (O fg) = O (map (map f) fg)
