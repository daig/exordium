{-# language MagicHash #-}
module X.Arrow.Traversed (module X.Arrow.Traversed, module X) where
import {-# source #-} X.Type.I
import {-# source #-} X.Type.K
import X.Functor.Traverse as X
import X.Arrow.Promap as X
import X.Arrow.Traversed.Internal
import X.ADT.E
import X.ADT.Maybe
import X.ADT.Where
import X.Functor.Swap
import X.Cast.Coerce.Unsafe

-- | Definitions in terms of @traversal@ are much more efficient
class (Traversed0 p, Traversed1 p) => Traversed p where
  {-# minimal traversal | traversed #-}
  -- | Lift a Van Laarhoven traversal into the profunctor
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  -- | Pass through the structure of any traversable container
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse


traversed_promap :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_promap f g = traversal (\xfy a -> map g (xfy (f a)))

traversed0__L :: Traversed0 p => p a b -> p (E a y) (E b y)
traversed0__L = \p -> promap swap swap (traversed0 p)
traversed0__R :: Traversed0 p => p a b -> p (E x a) (E x b)
traversed0__R = traversed0


-- Definitions in terms of @traversal1@ are much more efficient
class Traversed_ p => Traversed1 p where
  {-# minimal traversal1 | traversed1 #-}
  {-funList :: (s -> FunList a b t) -> p a b -> p s t-}
  -- | Lift a relevant Van Laarhoven traversal into the profunctor
  traversal1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal1 f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  -- | Pass through the structure of any nonempty traversable container.
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = traversal1 traverse1


{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}

-- | An profunctor that is strong over both the product and sum structure of Hask.
--
-- Allows lifting to target a single component nested in a a single branch of a Sum-of-Product structure.
class (Traversed' p, Traversed_ p) => Traversed0 p where
  -- | Lift an affine Van Laarhoven traversal into the profunctor
  traversal0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal0 afbsft = lens0 (\s -> swap (afbsft L s)) (\s b -> case afbsft (\_ -> I b) s of I t -> t)
  {-traversal0 f pab = promap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  -- | Pass through the product and/or sum structure of any affinely traversable container
  traversed0 :: Traverse0 t => p a b -> p (t a) (t b)
  traversed0 = traversal0 traverse0
  -- | Create an affine traversal ("Laser") from a match pattern and a setter
  lens0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  lens0 get set pab = promap (\s -> (get s, s)) (\(bt, s) -> bifoldMap_ (\x -> x) (set s) bt) (_1 (_R pab))



-- | A Strong profunctor with respect to the sum @E@.
--
-- ALlows lifting to target a single branch of a sum structure.
class Promap p => Traversed' p where
  {-# minimal prism | _R | _L #-}
  -- | Create an exact traversal ("Prism") from a match pattern and constructor
  prism :: (s -> E t a) -> (b -> t) -> p a b -> p s t
  prism pat constr = \p -> promap (\s -> swap (pat s)) (bifoldMap_ constr (\x -> x)) (_L p)
  {-traversal' :: forall s t b a. (forall f. Map f => (forall x. t -> f x) -> (a -> f b) -> s -> f t) -> p a b -> p s t-}
  {-traversal' tfxafbsft = prism (\s -> swap (tfxafbsft (\t -> R (coerce# t))  L s))-}
                               {-(\b -> case tfxafbsft (\t -> I (coerce# t)) (\_ -> I b) __ of I t -> t)-}
  {-traversed' :: forall f a b. Traverse' f => p a b -> p (f a) (f b)-}
  {-traversed' = prism (foldMap' (\fa -> L (mapCoerce# @b fa)) pure) pure-}
  {-traversed' l p = promap (l ) pure p-}
  -- | Pass through the right component of a sum
  _R :: p a b -> p (E x a) (E x b)
  {-_R = prism (\case L t -> L t; R a -> R a) R-}
  {-_R = traversal' (\tfx afb -> \case {L x -> L `map` tfx (L x); R a -> R `map` afb a})-}
  -- | Pass through the left component of a sum
  _L :: p a b -> p (E a y) (E b y)
  _L = \p -> promap swap swap (_R p)
  {-_L = traversal' (\tfx afb -> \case {R x -> R `map` tfx (R x); L a -> L `map` afb a})-}

instance Traversed' (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

-- | A Strong profunctor with respect to the product (,).
-- Allows lifting to target a single element of a product structure.
-- 
class Promap p => Traversed_ p where
  {-# minimal lens | traversal_ | traversed_ | _1 | _2 #-}
  -- | Create a linear traversal ("Lens") from a getter and a setter
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  -- lens get set = traversal_ (\afb s -> set s `map` afb (get s))
  lens get set = \p -> promap (\x -> (x,get x)) (\(s,b) -> set s b) (_2 p)
  -- | Lift a linear Van Laarhoven traversal into the profunctor
  traversal_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})
  -- | Pass through the product structure of any linearly traversable container
  traversed_ :: Traverse_ t => p a b -> p (t a) (t b)
  traversed_ = traversal_ traverse_
  -- | Pass through the second component of a product
  _2 :: p a b -> p (x,a) (x,b)
  -- _2 = \p -> promap swap swap (_1 p)
  _2 = traversed_
  -- | Pass through the first component of a product
  _1 :: p a b -> p (a,y) (b,y)
  -- _1 = lens (\(a,_) -> a) (\(_,c) b -> (b,c))
  --  _1 = traversal_ (\afb (a,y) -> (,y) `map` afb a)
  _1 = \p -> promap swap swap (_2 p)



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


($:) :: Traversed_ p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `postmap` _1 p


traversal__promap :: Traversed_ p => (a -> x) -> (y -> b) -> p x y -> p a b
traversal__promap f g = traversal_ (\xfy a -> map g (xfy (f a)))
