{-# language MagicHash #-}
module X.Arrow.Traversed.Internal.O (O(..), module X) where
import X.Type.I as X
import X.Functor.Applicative as X

newtype O f g a = O {unO :: f (g a)}

instance (Pure f,Pure g) => Pure (O f g) where pure a = O (pure (pure a))
instance (Apply f,Apply g) => FTimes (O f g) where ftimes = ap_ftimes
instance (Apply f,Apply g) => Apply (O f g) where O fgf `ap`O fga = O (map ap fgf `ap` fga)
instance (Applicative f, Applicative g) => Applicative (O f g)
instance (Map f, Strong g) => Strong (O f g) where strong a (O fg) = O (map (strong a) fg)
instance (Map f,Map g) => Map (O f g) where
  map f (O fg) = O (map (map f) fg)
  map# f (O fg) = O ((map## f) fg)
  map## f hofg = map# O (unO (map## f (O (map# unO hofg)))) -- Yikes
instance (Remap f,Remap g) => Remap (O f g) where
  remap f g (O fg) = O (remap (remap g f) (remap f g) fg)
