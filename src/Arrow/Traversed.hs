{-# language MagicHash #-}
module Arrow.Traversed (module Arrow.Traversed, module X) where
import {-# source #-} Type.I
import {-# source #-} Type.K
import Functor.Traverse as X
import Arrow.Promap as X
import Arrow.Traversed.Internal
import ADT.E
import ADT.Maybe
import ADT.Where
import Functor.Swap
import Cast.Coerce.Unsafe

class (Traversed0 p, Traversed1 p) => Traversed p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse


traversed_promap :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_promap f g = traversal (\xfy a -> map g (xfy (f a)))

traversed0__L :: Traversed0 p => p a b -> p (E a y) (E b y)
traversed0__L = \p -> promap swap swap (traversed0 p)
traversed0__R :: Traversed0 p => p a b -> p (E x a) (E x b)
traversed0__R = traversed0

--itraverseOf' :: (IFun f Int a b -> IFun f Int s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf' l iafb = case l (IFun iafb) of IFun isft -> isft 0

{-type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)-}
{-type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)-}

{-type (s ~*.  a) b t = A Traversed_ a b a b -> A Traversed_ a b s t-}
{-type  s ~**. a = A Traversed_ a a a a -> A Traversed_ a a s s-}

{-withTraversed_ :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_ l f = case l (A Traversed_ (\x -> x) (\_ b -> b)) of A Traversed_ x y -> f x y-}

{-cloneTraversed_ :: (s ~*. a) b t -> (s ~* a) b t-}
{-cloneTraversed_ l = withTraversed_ l (\x y p -> lens x y p)-}

{-withTraversed_' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_' -}

type (s ~*  a) b t = forall p. Traversed_ p => p a b -> p s t

($:) :: Traversed_ p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `postmap` _1 p


{-ff :: (s -> FunList a b t) -> (forall f. Apply f => (a -> f b) -> s -> f t)-}
{-ff sabt afb s = case sabt s of-}
  {-Done t -> gt-}
class Traversed_ p => Traversed1 p where
  {-# minimal traversal1 | traversed1 #-}
  {-funList :: (s -> FunList a b t) -> p a b -> p s t-}
  traversal1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal1 f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = traversal1 traverse1


{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}

class (Traversed' p, Traversed_ p) => Traversed0 p where
  traversal0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal0 afbsft = lens0 (\s -> swap (afbsft L s)) (\s b -> case afbsft (\_ -> I b) s of I t -> t)
  {-traversal0 f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  traversed0 :: Traverse0 t => p a b -> p (t a) (t b)
  traversed0 = traversal0 traverse0
  lens0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  lens0 get set pab = promap (\s -> (get s, s)) (\(bt, s) -> bifoldMap_ (\x -> x) (set s) bt) (_1 (_R pab))

{-type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t-}
{-type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s-}


{-choose :: (Traverse0 f, Pure f, Promap p) => p (E x a) (E x b) -> p (f a) (f b)-}
{-choose p = promap __ (foldMap0 pure) p-}
class Promap p => Traversed' p where
  {-# minimal prism | traversed' | _R | _L #-}
  prism :: (s -> E t a) -> (b -> t) -> p a b -> p s t
  prism pat constr = \p -> promap (\s -> swap (pat s)) (bifoldMap_ constr (\x -> x)) (_L p)
  {-traversal' :: forall s t b a. (forall f. Map f => (forall x. t -> f x) -> (a -> f b) -> s -> f t) -> p a b -> p s t-}
  {-traversal' tfxafbsft = prism (\s -> swap (tfxafbsft (\t -> R (coerce# t))  L s))-}
                               {-(\b -> case tfxafbsft (\t -> I (coerce# t)) (\_ -> I b) __ of I t -> t)-}
  traversed' :: forall f a b. Traverse' f => p a b -> p (f a) (f b)
  traversed' = prism (foldMap' (\fa -> L (mapCoerce# @b fa)) pure) pure
  {-traversed' l p = promap (l ) pure p-}
  _R :: p a b -> p (E x a) (E x b)
  _R = traversed'
  {-_R = traversal' (\tfx afb -> \case {L x -> L `map` tfx (L x); R a -> R `map` afb a})-}

  {-((forall x. g b -> f x) -> (a -> f b) -> g a -> f (g b)) -> p a b -> p (g a) (g b)-}
  _L :: p a b -> p (E a y) (E b y)
  _L = \p -> promap swap swap (_R p)
  {-_L = traversal' (\tfx afb -> \case {R x -> R `map` tfx (R x); L a -> L `map` afb a})-}

instance Traversed' (->) where
  {-prism pat constr f s = foldMap' (\case L t -> t) (\a -> constr (f a)) (pat s)-}
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

class Promap p => Traversed_ p where
  {-# minimal lens | traversal_ | traversed_ | _1 | _2 #-}
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  lens get set = \p -> promap (\x -> (x,get x)) (\(s,b) -> set s b) (_2 p)
  {-lens get set = traversal_ (\afb s -> set s `map` afb (get s))-}
  traversal_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})
  traversed_ :: Traverse_ t => p a b -> p (t a) (t b)
  traversed_ = traversal_ traverse_
  _2 :: p a b -> p (x,a) (x,b)
  {-_2 = \p -> promap swap swap (_1 p)-}
  _2 = traversed_
  _1 :: p a b -> p (a,y) (b,y)
  {-_1 = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {-_1 = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  _1 = \p -> promap swap swap (_2 p)

traversal__promap :: Traversed_ p => (a -> x) -> (y -> b) -> p x y -> p a b
traversal__promap f g = traversal_ (\xfy a -> map g (xfy (f a)))


{-instance Optic Traversed_ where data A Traversed_ a b s t = Traversed_ (s -> a) (s -> b -> t)-}
{-instance Traversed_ (A Traversed_ a b) where-}
  {-_1 (Traversed_ x y) = Traversed_ (\(a,_) -> x a) (\(s,c) b -> (y s b,c))-}
{-instance Promap (A Traversed_ a b) where-}
  {-promap f g (Traversed_ x y) = Traversed_ (\s -> x (f s)) (\s b -> g (y (f s) b))-}

{-data A Traversed_ p a b t = Pretext {runPretext :: forall f. Map f => p a (f b) -> f t}-}

{-instance Traversed' (->) where-}
  {-traversed' l f s = case l (\a -> I (f a)) s of {I t -> t}-}
instance Traversed_ (->) where
  lens sa sbt ab = \s -> sbt s (ab (sa s))
  _1 f (a,x) = (f a,x)
  _2 f (x,a) = (x, f a)
instance Traversed1 (->) where traversal1 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Traversed0 (->) where traversal0 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Traversed (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

class Promap p => Cochoice p where
  {-# minimal un_L | un_R #-}
  un_L :: p (E a y) (E b y) -> p a b
  un_L p = un_R (promap swap swap p)
  un_R :: p (E x a) (E x b) -> p a b
  un_R p = un_L (promap swap swap p)
{-instance Prism p -}
