module O (O(..),module X) where
import Pure as X
import I

newtype O f g a = O (f (g a))
instance (Map f,Map g) => Map (O f g) where map f (O fg) = O (map (map f) fg)
instance (Pure f,Pure g) => Pure (O f g) where pure a = O (pure (pure a))
instance (Apply f,Apply g) => Apply (O f g) where O fgf @$@ O fga = O (fgf |@(@$@)@| fga)
instance (Applicative f, Applicative g) => Applicative (O f g)
type family Stacked (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  Stacked '[] = I
  Stacked (f ': fs) = O f (Stacked fs)
