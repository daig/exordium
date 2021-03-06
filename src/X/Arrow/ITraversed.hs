{-# language MagicHash #-}
{-# language UndecidableSuperClasses #-}
module X.Arrow.ITraversed (module X.Arrow.ITraversed, module X) where
import X.Type.I
import X.Type.K
import X.Functor.ITraverse as X
import X.Arrow.Indexed as X
import X.Arrow.Compose as X
import X.Functor.Swap
{-import X.Arrow.ITraversed.Internal-}

-- | Definitions in terms of @itraversal@ are much more efficient
class (ITraversed0 i q p, ITraversed1 i q p) => ITraversed i q p where
  {-{-# minimal itraversal | itraversed #-}-}
  -- | Lift an indexed Van Laarhoven traversal into the profunctor
  itraversal :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t) -> p a b -> q s t
  -- | Pass through the structure of any indexed traversable container
  itraversed :: ITraverse i t => p a b -> q (t a) (t b)
  itraversed = itraversal (itraverse @i)



-- Definitions in terms of @itraversal1@ are much more efficient
class ITraversed_ i q p => ITraversed1 i q p where
  {-# minimal itraversal1 #-}
  -- | Lift a relevant Van Laarhoven itraversal into the profunctor
  itraversal1 :: (forall f. Apply f => (i -> a -> f b) -> s -> f t) -> p a b -> q s t
  -- | Pass through the structure of any nonempty indexed traversable container.
  itraversed1 :: ITraverse1 i t => p a b -> q (t a) (t b)
  itraversed1 = itraversal1 (itraverse1 @i)


-- | An profunctor whcih can carry Indexed Traversals
--
-- Allows lifting to target a single component nested in a a single branch of a Sum-of-Product structure.
class (ITraversed' i q p, ITraversed_ i q p) => ITraversed0 i q p where
  {-# minimal itraversal0 | ilens0 #-}
  {--- | Lift an affine indexed Van Laarhoven traversal into the profunctor-}
  itraversal0 :: (forall f. Pure f => (i -> a -> f b) -> s -> f t) -> p a b -> q s t
  itraversal0 iafbsft = ilens0 (\s -> swap (iafbsft (\i a -> L (i,a)) s))
                               (\s b -> case iafbsft (\_ _ -> I b) s of I t -> t)
  {--- | Create an affine indexed traversal ("Indexed Laser") from a match pattern and a setter-}
  ilens0 :: (s -> E t (i,a)) -> (s -> b -> t) -> p a b -> q s t
  ilens0 pat set = itraversal0 (\iafb s -> case pat s of
    L t -> pure t
    R (i,a) -> set s `map` iafb i a)
  {-ilens0 get set pab = promap (\s -> (get s, s)) (\(bt, s) -> bifoldMap_ (\x -> x) (set s) bt) (_1 (_R pab))-}



-- | A Strong profunctor with respect to the sum @E@.
--
-- ALlows lifting to target a single branch of a sum structure.
class PIndexed i q p => ITraversed' i q p | p -> q where
  {-{-# minimal prism | _R | _L #-}-}
  {--- | Create an exact itraversal ("Prism") from a match pattern and constructor-}
  iprism :: (s -> E t (i,a)) -> (b -> t) -> p a b -> q s t
  {-prism pat constr = \p -> promap (\s -> swap (pat s)) (bifoldMap_ constr (\x -> x)) (_L p)-}
  {-itraversal' :: forall s t b a. (forall f. Map f => (forall x. t -> f x) -> (i -> a -> f b) -> s -> f t) -> p a b -> p s t-}
  {-itraversal' tfxafbsft = prism (\s -> swap (tfxafbsft (\t -> R (coerce# t))  L s))-}
                               {-(\b -> case tfxafbsft (\t -> I (coerce# t)) (\_ -> I b) __ of I t -> t)-}
  {-itraversed' :: forall f a b. Traverse' f => p a b -> p (f a) (f b)-}
  {-itraversed' = prism (foldMap' (\fa -> L (mapCoerce# @b fa)) pure) pure-}
  {-itraversed' l p = promap (l ) pure p-}
  -- | Pass through the right component of a sum
  {-_iR :: p a b -> q (E x a) (E x b)-}
  {-_R = iprism (\case L t -> L t; R a -> R (R (),a)) R-}
  {-_R = itraversal' (\tfx afb -> \case {L x -> L `map` tfx (L x); R a -> R `map` afb a})-}
  -- | Pass through the left component of a sum
  {-_iL :: p a b -> q (E a y) (E b y)-}
  {-_L = \p -> promap swap swap (_R p)-}
  {-_L = itraversal' (\tfx afb -> \case {R x -> R `map` tfx (R x); L a -> L `map` afb a})-}

{-instance ITraversed' i (->) (->) where-}
  {-prism pat constr f s = case pat s of-}
    {-L t -> t-}
    {-R a -> constr (f a)-}

-- | A Strong profunctor with respect to the product (,).
-- Allows lifting to target a single element of a product structure.
-- 
class (Traversed_ p, PIndexed i q p) => ITraversed_ i q p | p -> q where
--   {-# minimal lens | itraversal_ | itraversed_ | _1 | _2 #-}
  -- | Create a linear itraversal ("Lens") from an index, a getter, and a setter
  ilens :: (s -> i) -> (s -> a) -> (s -> b -> t) -> p a b -> q s t
  ilens i get set = itraversal_ (\iafb s -> set s `map` iafb (i s) (get s))
  -- | Lift a linear Van Laarhoven itraversal into the profunctor
  itraversal_ :: (forall f. Map f => (i -> a -> f b) -> s -> f t) -> p a b -> q s t
  itraversal_ l = ilens (\s -> case l (\i _ -> K i) s of K i -> i)
                        (\s -> case l (\_ a -> K a) s of K a -> a)
                        (\s b -> case l (\_ _ -> I b) s of I t -> t)
  -- | Pass through the product structure of any linearly traversable container
  itraversed_ :: ITraverse_ i t => p a b -> q (t a) (t b)
  itraversed_ = itraversal_ (itraverse_ @i)



instance ITraversed' i (->) (->) where iprism = prism_iprism
instance ITraversed0 i (->) (->) where itraversal0 = traversal0_itraversal0
instance ITraversed_ i (->) (->) where itraversal_ = traversal__itraversal_
instance ITraversed1 i (->) (->) where itraversal1 = traversal1_itraversal1
instance ITraversed  i (->) (->) where itraversal = traversal_itraversal


prism_iprism :: Traversed' p => (s -> E t (i,a)) -> (b -> t) -> p a b -> p s t
prism_iprism pat = prism (map (map (\(_,a) -> a)) pat)
traversal__itraversal_ :: Traversed_ p => (forall f. Map f => (i -> a -> f b) -> s -> f t) -> p a b -> p s t
traversal__itraversal_ iafbsft = traversal_ (\afb -> iafbsft (\_ -> afb))
traversal0_itraversal0 :: Traversed0 p => (forall f. Pure f => (i -> a -> f b) -> s -> f t) -> p a b -> p s t
traversal0_itraversal0 iafbsft = traversal0 (\afb -> iafbsft (\_ -> afb))
traversal1_itraversal1 :: Traversed1 p => (forall f. Apply f => (i -> a -> f b) -> s -> f t) -> p a b -> p s t
traversal1_itraversal1 iafbsft = traversal1 (\afb -> iafbsft (\_ -> afb))
traversal_itraversal :: Traversed p => (forall f. Applicative f => (i -> a -> f b) -> s -> f t) -> p a b -> p s t
traversal_itraversal iafbsft = traversal (\afb -> iafbsft (\_ -> afb))

{-instance Traversed_ p => Traversed_ (IP i p) where traversal_ l (IP ip) = IP (\i -> traversal_ l (ip i))-}
{-instance (Traversed_ p, PIndexed i p p, Compose p) => ITraversed_ i p (IP i p) where-}
  {-itraversing_ l (IP ip) = l (\i a -> (ip i))-}
