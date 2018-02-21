module O (module O, module X) where
import {-# source #-} I as X

newtype O f g a = O (f (g a))

type family O_ (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  O_ '[] = I
  O_ (f ': fs) = O f (O_ fs)

{-instance (Pure f,Pure g) => Pure (O f g) where pure a = O (pure (pure a))-}
{-instance (Apply f,Apply g) => Apply (O f g) where O fgf |$| O fga = O (fgf |$(|$|)$| fga)-}
{-instance (Applicative f, Applicative g) => Applicative (O f g)-}
