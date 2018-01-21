module APrism (module APrism, module X) where
import APrism.Type as X
import E as X
import Prelude ((.))

type APrism' a = APrism a a

aprism'prism pat constr (APrism pat' constr' ) = APrism f (constr . constr') where
  f s = case pat s of
    L t -> L t
    R a -> case pat' a of
      L b -> L (constr b)
      R a' -> R a'
{-# inline aprism'prism #-}
aprism'left (APrism seta bt) = (`APrism` (L . bt)) (\case
  L s -> case seta s of
    L t -> L (L t)
    R a -> R a
  R c -> L (R c))
{-# inline aprism'left #-}
aprism'right (APrism seta bt) = (`APrism` (R . bt)) (\case
  L c -> L (L c)
  R s -> case seta s of
    L t -> L (R t)
    R a -> R a)
{-# inline aprism'right #-}
aprism'dimap f g (APrism seta bt) = APrism (e'bifoldMap_ (L . g) R . seta . f) (g . bt)
aprism'premap f (APrism seta bt) = APrism (seta . f) bt
aprism'postmap f (APrism seta bt) = APrism (e'bifoldMap_ (L . f) R . seta) (f . bt)
aprism'map = aprism'postmap
aprism'mapIso = aprism'map
aprism'pure t = APrism (\_ -> L t) (\_ -> t)

