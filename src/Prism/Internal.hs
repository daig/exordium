module Prism.Internal (module Prism.Internal, module X) where
import E.Utils as X
import Prelude ((.))
import Pure as X
import Prism.Class

data APrism a b s t = APrism (s -> E t a) (b -> t)
type APrism' a = APrism a a

instance Prism (APrism a b) where
  prism pat constr (APrism pat' constr' ) = APrism f (constr . constr') where
    f s = case pat s of
      L t -> L t
      R a -> case pat' a of
        L b -> L (constr b)
        R a' -> R a'
  right (APrism seta bt) = (`APrism` (R . bt)) (\case
    L c -> L (L c)
    R s -> case seta s of
      L t -> L (R t)
      R a -> R a)
  left (APrism seta bt) = (`APrism` (L . bt)) (\case
    L s -> case seta s of
      L t -> L (L t)
      R a -> R a
    R c -> L (R c))
instance Dimap (APrism a b) where
  dimap f g (APrism seta bt) = APrism (e'bifoldMap (L . g) R . seta . f) (g . bt)
instance ComapL (APrism a b) where
  colmap f (APrism seta bt) = APrism (seta . f) bt
instance MapR (APrism a b) where rmap = map
instance Map (APrism a b s) where
 map f (APrism seta bt) = APrism (e'bifoldMap (L . f) R . seta) (f . bt)
instance MapIso (APrism a b s) where mapIso _ = map
instance Pure (APrism a b s) where pure t = APrism (\_ -> L t) (\_ -> t)
