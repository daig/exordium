module Optic.Prism (module Optic.Prism, module X) where
import Prelude ((.))
import Pure as X
import Traversed
import Maybe as X
import Optic.View as X
import Optic.Review as X
import FoldMap.Bi

data Prism a b s t = Prism (s -> E t a) (b -> t)
type Prism' a = Prism a a

{-type (s ~+. a) b t = Prism a b a b -> Prism a b s t-}
{-withPrism :: (s ~+. a) b t -> ((s -> E t a) -> (b -> t) -> r) -> r-}
_Prism :: (Prism a b a b -> Prism a b s t) -> ((s -> E t a) -> (b -> t) -> r) -> r
_Prism l k = case l (Prism R (\x -> x)) of Prism h g -> k h g

match :: (Prism a b a b -> Prism a b s t) -> (t -> r) -> (a -> r) -> s -> r
match l kt ka = _Prism l (\pat _ -> \s -> case pat s of {L t -> kt t; R a -> ka a})

match' :: (Prism a b a b -> Prism a b s t) -> r -> (a -> r) -> s -> r
match' l r ka = _Prism l (\pat _ -> \s -> case pat s of {L _ -> r; R a -> ka a})

view' :: (Prism a b a b -> Prism a b s t) -> a -> s -> a
view' l def = match' l def (\a -> a)

preview :: (Prism a b a b -> Prism a b s t) -> s -> Maybe a
preview l = match' l Nothing Just

_Just :: Traversed' p => p a b -> p (Maybe a) (Maybe b)
_Just = prism (\s -> case s of {Nothing -> L Nothing; Just a -> R a}) Just

prism' :: (s -> Maybe a) -> (b -> s) -> Prism a b a b -> Prism a b s s
prism' sma bs = prism (\s -> case sma s of {Just a -> R a; Nothing -> L s}) bs

instance Traversed' (Prism a b) where
  prism pat constr (Prism pat' constr' ) = Prism f (constr . constr') where
    f s = case pat s of
      L t -> L t
      R a -> case pat' a of
        L b -> L (constr b)
        R a' -> R a'
  right (Prism seta bt) = (`Prism` (R . bt)) (\case
    L c -> L (L c)
    R s -> case seta s of
      L t -> L (R t)
      R a -> R a)
  left (Prism seta bt) = (`Prism` (L . bt)) (\case
    L s -> case seta s of
      L t -> L (L t)
      R a -> R a
    R c -> L (R c))
instance Promap (Prism a b) where
  promap f g (Prism seta bt) = Prism (bifoldMap_ (L . g) R . seta . f) (g . bt)
instance Map (Prism a b s) where
 map f (Prism seta bt) = Prism (bifoldMap_ (L . f) R . seta) (f . bt)
instance Pure (Prism a b s) where pure t = Prism (\_ -> L t) (\_ -> t)
