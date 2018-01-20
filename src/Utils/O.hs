module Utils.O (module Utils.O, module X) where
import Type.O as X
import Type.I as X

type family Stacked (fs :: [* -> *]) = (f :: * -> *) | f -> fs where
  Stacked '[] = I
  Stacked (f ': fs) = O f (Stacked fs)
