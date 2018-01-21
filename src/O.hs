module O (module O, module X) where
import O.Type as X
import I.Type as X

type family Stacked (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  Stacked '[] = I
  Stacked (f ': fs) = O f (Stacked fs)
