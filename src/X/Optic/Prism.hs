module X.Optic.Prism (module X.Optic.Prism, module X) where
import Prelude ((.))
import X.Functor.Pure as X
import X.Arrow.Loop as X
import X.Arrow.Traversed
import X.Data.Maybe as X
import X.Optic.View as X
import X.Optic.Review as X
import X.Functor.Bifold

-- | A @Prism@ is like a first class pattern synonym, consisting of a possible match, and a cosntructor
data Prism a b s t = Prism (s -> E t a) (b -> t)

{-type (s ~+. a) b t = Prism a b a b -> Prism a b s t-}
{-withPrism :: (s ~+. a) b t -> ((s -> E t a) -> (b -> t) -> r) -> r-}
_Prism :: (Prism a b a b -> Prism a b s t) -> ((s -> E t a) -> (b -> t) -> r) -> r
_Prism l k = case l (Prism R (\x -> x)) of Prism h g -> k h g

-- | Use a @Prism@ in a case-like match, providing a continuation for what to do if it does match, or if it doesn't
match :: (Prism a b a b -> Prism a b s t) -> (t -> r) -> (a -> r) -> s -> r
match l kt ka = _Prism l (\pat _ -> \s -> case pat s of {L t -> kt t; R a -> ka a})

-- | Use a @Prism@ in a simple case-like match, providing a default result for if it doesn't match, and a continuation for what to do if it does.
match' :: (Prism a b a b -> Prism a b s t) -> r -> (a -> r) -> s -> r
match' l r ka = _Prism l (\pat _ -> \s -> case pat s of {L _ -> r; R a -> ka a})

-- | Try to view the target of a @Prism@, providing a default value if it's empty
view' :: (Prism a b a b -> Prism a b s t) -> a -> s -> a
view' l def = match' l def (\a -> a)

-- | Try to view the target of a @Prism@
preview :: (Prism a b a b -> Prism a b s t) -> s -> Maybe a
preview l = match' l Nothing Just

-- | An example prism for matching the @Just@ constructor
_Just :: forall a b p. Traversed' p => p a b -> p (Maybe a) (Maybe b)
_Just = prism (\s -> case s of {Nothing -> L Nothing; Just a -> R a}) Just

-- | Construct a simple prism
prism' :: Traversed' p => (s -> Maybe a) -> (b -> s) -> p a b -> p s s
prism' sma bs = prism (\s -> case sma s of {Just a -> R a; Nothing -> L s}) bs

instance Traversed' (Prism a b) where
  prism pat constr (Prism pat' constr' ) = Prism f (constr . constr') where
    f s = case pat s of
      L t -> L t
      R a -> case pat' a of
        L b -> L (constr b)
        R a' -> R a'
  _R (Prism seta bt) = (`Prism` (R . bt)) (\case
    L c -> L (L c)
    R s -> case seta s of
      L t -> L (R t)
      R a -> R a)
  _L (Prism seta bt) = (`Prism` (L . bt)) (\case
    L s -> case seta s of
      L t -> L (L t)
      R a -> R a
    R c -> L (R c))
instance Promap (Prism a b) where
  promap f g (Prism seta bt) = Prism (bifoldMap_ (L . g) R . seta . f) (g . bt)
instance Map (Prism a b s) where
 map f (Prism seta bt) = Prism (bifoldMap_ (L . f) R . seta) (f . bt)
instance Remap (Prism a b s) where remap _ = map
instance Pure (Prism a b s) where pure t = Prism (\_ -> L t) (\_ -> t)

{-type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t-}
{-type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s-}
instance Loop' (Prism a a) where
  loopLeft (Prism sy'tya a'ty) = Prism (\s -> loop1 (L s)) loop2  where
    loop1 sy = case sy'tya sy of
      L (L t) -> L t
      L (R y) -> loop1 (R y)
      R a -> R a
    loop2 a = case a'ty a of
      L t -> t
      R y -> case loop1 (R y) of
        L t -> t
        R a' -> loop2 a'
