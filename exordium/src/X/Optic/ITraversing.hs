module X.Optic.ITraversing (module X.Optic.ITraversing, module X) where
import X.Optic.Traversing as X
import X.Arrow.ITraversed as X
import X.Arrow.Sieve
import X.Arrow.Tabulated

{- 
newtype ITraversing i f a b = ITraversing {runITraversing :: i -> a -> f b}
  {-deriving anyclass Representable-}

-- | Lift an f-operation over the target of a traversal
_ITraversing :: (ITraversing i f a b -> Traversing f s t) -> (i -> a -> f b) -> s -> f t
_ITraversing l iafb s = case l (ITraversing iafb) of Traversing sft -> sft s
-- | Same as _ITraversing
itraversing :: Applicative f => (ITraversing i f a b -> Traversing f s t) 
                             -> (i -> a -> f b)      -> s -> f t
itraversing = coerce



{-instance Bind m => Compose (Traversing m) where Traversing f > Traversing g = Traversing (g <=< f)-}
{-instance Monad m => Category (Traversing m) where id = Traversing pure-}
instance Zip f => Closed (ITraversing i f) where
  closed (ITraversing iafb) = ITraversing (\i xa -> distribute (\x -> iafb i (xa x)))
instance Map f => Promap (ITraversing i f) where promap f g (ITraversing is) = ITraversing (\i -> (promap f (map g) (is i)))
instance Strong f => Strong (ITraversing i f a) where
  strong x (ITraversing iafb) = ITraversing (\i a -> strong x (iafb i a))
instance Map f => Map (ITraversing i f a) where map f (ITraversing is) = ITraversing (\i a -> map f (is i a))
instance Remap f => Remap (ITraversing i f a) where
  remap f g (ITraversing is) = ITraversing \i a -> remap f g (is i a)
{-{--- TODO: move to PromapIso class-}-}
instance Comap f => Comap (ITraversing i f a) where comap f (ITraversing is) = ITraversing \i a -> comap f (is i a)
instance Coerce1 f => Folded_ (ITraversing i f) where postcoerce (ITraversing is) = ITraversing \i a -> coerce1 (is i a)

instance Map f => Traversed_ (ITraversing i f) where traversal_ afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Pure f => Traversed' (ITraversing i f) where
  prism pat constr (ITraversing iafb) = ITraversing (\i s -> case pat s of
    L t -> pure t
    R a -> constr `map` iafb i a)
instance Apply f => Traversed1 (ITraversing i f) where
  traversal1 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Pure f => Traversed0 (ITraversing i f) where
  traversal0 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)
instance Applicative f => Traversed (ITraversing i f) where
  traversal afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)

{-{-instance Indexed i (Traversing f) where indexed s _ = s-}-}

instance Zip f => Mapped (ITraversing i f) where
   mapped (ITraversing f) = ITraversing (\i -> collect (f i))


{---itraverseOf :: Applicative f => (IndexingP (Traversing f) a b -> IndexingP (Traversing f) s t) -> (Int -> a -> f b) -> s -> f t-}
{---itraverseOf l iafb = runTraversing (indexP (l (IndexingP (\i -> (i `add` (1::Int),Traversing (iafb i))))) (0::Int))-}

{-{-collectOf :: (Traversing f a b -> Traversing f s t) -> (a -> f b) -> s -> f t-}-}
{-{-collectOf g f = case g (Traversing f) of Traversing f' -> f'-}-}

{---itraverseOf' :: (IFun f Int a b -> IFun f Int s t) -> (Int -> a -> f b) -> s -> f t-}
{---itraverseOf' l iafb = case l (IFun iafb) of IFun isft -> isft 0-}


{-{-withTraversed_ :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}-}
{-{-withTraversed_ l f = case l (A Traversed_ (\x -> x) (\_ b -> b)) of A Traversed_ x y -> f x y-}-}

{-{-cloneTraversed_ :: (s ~*. a) b t -> (s ~* a) b t-}-}
{-{-cloneTraversed_ l = withTraversed_ l (\x y p -> lens x y p)-}-}

{-{-withTraversed_' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}-}
{-{-withTraversed_' -}-}

{-{-type (s ~*  a) b t = forall p. Traversed_ p => p a b -> p s t-}-}

type instance Rep (ITraversing i f) = Traversing f i
instance Map f => Sieve (ITraversing i f) where
  sieve (ITraversing ifab) a = Traversing (\i -> ifab i a)
instance Map f => Tabulated (ITraversing i f) where
  tabulateP aifb = ITraversing (\i a -> case aifb a of Traversing ifb -> ifb i)

{-icompose :: (i -> j -> k) -> (ITraversing i f s t -> r) -> (ITraversing j f a b -> Traversing f s t) -> (ITraversing k f a b -> r)-}
{-icompose ijk istr jabst pab = istr (ITraversing (\i s -> jabst (ITraversing (\j -> (runITraversing pab (ijk i j)))) `runTraversing` s))-}

icompose :: (i -> j -> k) -> (ITraversing i f s t -> r) -> (ITraversing j f a b -> Traversing f s t) -> (ITraversing k f a b -> r)
icompose ijk istr jabst pab = istr (ITraversing (\i s -> jabst (ITraversing (\j -> (runITraversing pab (ijk i j)))) `runTraversing` s))

rcompose :: (PIndexed k q p, Sieve q)
         => (i -> j -> k)
         -> (ITraversing i (Rep q) s t -> r)
         -> (ITraversing j (Rep q) a b -> q s t)
         -> (p a b -> r)
rcompose ijk istr jabst pab = istr (ITraversing (\i -> sieve (jabst (ITraversing (\j -> sieve (pix pab (ijk i j)))))))
{-newtype IP i p a b = IP {runIP :: i -> p a b}-}
{-instance Promap p => Promap (IP i p) where promap f g (IP ip) = IP (\i -> promap f g (ip i))-}
{-instance PIndexed i p p => PIndexed i p (IP i p) where pix = runIP-}

{-pcompose :: PIndexed k q p => (i -> j -> k) -> (IP i q s t -> r) -> (IP j q a b -> q s t) -> (p a b -> r)-}
{-pcompose ijk istr jabst pab = istr (IP (\i -> jabst (IP (\j -> pix pab (ijk i j)))))-}
instance Map f => PIndexed i (Traversing f) (Traversing f)
instance (i ~ j, Map f) => PIndexed i (Traversing f) (ITraversing j f) where
  pix (ITraversing f) i = Traversing (f i)

instance Pure f => ITraversed' i (Traversing f) (ITraversing i f) where
  iprism pat constr (ITraversing iafb) = Traversing (\s -> case pat s of
      L t -> pure t
      R (i,a) -> constr `map` iafb i a)
instance Applicative f => ITraversed i (Traversing f) (ITraversing i f) where
  itraversal l (ITraversing f) = Traversing (\s -> l (\i a -> (f i a)) s)
instance Pure f => ITraversed0 i (Traversing f) (ITraversing i f) where
  itraversal0 l (ITraversing f) = Traversing (\s -> l (\i a -> (f i a)) s)
instance Apply f => ITraversed1 i (Traversing f) (ITraversing i f) where
  itraversal1 l (ITraversing f) = Traversing (\s -> l (\i a -> (f i a)) s)
instance Map f => ITraversed_ i (Traversing f) (ITraversing i f) where
  itraversal_ l (ITraversing f) = Traversing (\s -> l (\i a -> (f i a)) s)
  ilens si sa sbt (ITraversing iafb) = Traversing (\s -> sbt s `map` iafb (si s) (sa s))

instance Pure f => ITraversed' i (Traversing f) (Traversing f) where iprism = prism_iprism
instance Pure f => ITraversed0 i (Traversing f) (Traversing f) where itraversal0 = traversal0_itraversal0
instance Map f => ITraversed_ i (Traversing f) (Traversing f) where itraversal_ = traversal__itraversal_
instance Apply f => ITraversed1 i (Traversing f) (Traversing f) where itraversal1 = traversal1_itraversal1
instance Applicative f => ITraversed  i (Traversing f) (Traversing f) where itraversal = traversal_itraversal
-}
