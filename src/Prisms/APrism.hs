module Prisms.APrism (module Prisms.APrism, module X) where
import Sum as X (E)
import Sum
import Prism as X
import Pure as X
import Prelude ((.))

data APrism a b s t = APrism (s -> E t a) (b -> t)
type APrism' a = APrism a a

instance Dimap (APrism a b) where
  dimap f g (APrism seta bt) = APrism (either (L . g) R . seta . f) (g . bt)
  {-# INLINE dimap #-}
  premap f (APrism seta bt) = APrism (seta . f) bt
  {-# INLINE premap #-}
  postmap f (APrism seta bt) = APrism (either (L . f) R . seta) (f . bt)
  {-# INLINE postmap #-}
instance MapIso (APrism a b s) where mapIso = map_mapIso
instance Map (APrism a b s) where map = postmap
instance Pure (APrism a b s) where pure t = APrism (\_ -> L t) (\_ -> t)

instance Prism (APrism a b) where
  -- TODO: check correctness
  prism pat constr (APrism pat' constr' ) = APrism f (constr . constr') where
    f s = case pat s of
      L t -> L t
      R a -> case pat' a of
        L b -> L (constr b)
        R a' -> R a'

  left (APrism seta bt) = (`APrism` (L . bt)) (\case
    L s -> case seta s of
      L t -> L (L t)
      R a -> R a
    R c -> L (R c))
  {-# INLINE left #-}
  right (APrism seta bt) = (`APrism` (R . bt)) (\case
    L c -> L (L c)
    R s -> case seta s of
      L t -> L (R t)
      R a -> R a)
  {-# INLINE right #-}
