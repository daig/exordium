module X.Optic.Traversing (module X.Optic.Traversing, module X) where
import X.Arrow.Sieve
import X.Arrow.Tabulated
{-import X.Indexable.Class as X-}
import X.Arrow.Folded as X
import X.Arrow.Mapped as X
import X.Functor.Coerce1 as X
{-import X.Arrow.Representable as X-}

newtype Traversing f a b = Traversing {runTraversing :: a -> f b}
  {-deriving anyclass Representable-}

-- | Lift an f-operation over the target of a traversal
_Traversing :: (Traversing f a b -> Traversing f s t) -> (a -> f b) -> s -> f t
_Traversing l afb s = case l (Traversing afb) of Traversing sft -> sft s

{-instance Bind m => Compose (Traversing m) where Traversing f > Traversing g = Traversing (g <=< f)-}
{-instance Monad m => Category (Traversing m) where id = Traversing pure-}
instance Zip f => Closed (Traversing f) where
  closed (Traversing afb) = Traversing (\xa -> distribute (\x -> afb (xa x)))
instance Map f => Promap (Traversing f) where promap f g (Traversing s) = Traversing (promap f (map g) s)
instance Map f => Map (Traversing f a) where map f (Traversing s) = Traversing (\a -> map f (s a))
{--- TODO: move to PromapIso class-}
instance Comap f => Comap (Traversing f a) where comap f (Traversing s) = Traversing (\a -> comap f (s a))
instance Coerce1 f => Folded_ (Traversing f) where postcoerce (Traversing s) = Traversing (\a -> coerce1 (s a))

instance Map f => Traversed_ (Traversing f) where traversal_ afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)
instance Pure f => Traversed' (Traversing f) where
  prism pat constr (Traversing afb) = Traversing (\s -> case pat s of
    L t -> pure t
    R a -> constr `map` afb a)
instance Apply f => Traversed1 (Traversing f) where
  traversal1 afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)
instance Pure f => Traversed0 (Traversing f) where
  traversal0 afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)
instance Applicative f => Traversed (Traversing f) where
  traversal afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)

{-instance Indexed i (Traversing f) where indexed s _ = s-}

instance Zip f => Mapped (Traversing f) where
   mapped (Traversing f) = Traversing (collect f)


--itraverseOf :: Applicative f => (IndexingP (Traversing f) a b -> IndexingP (Traversing f) s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf l iafb = runTraversing (indexP (l (IndexingP (\i -> (i `add` (1::Int),Traversing (iafb i))))) (0::Int))

{-collectOf :: (Traversing f a b -> Traversing f s t) -> (a -> f b) -> s -> f t-}
{-collectOf g f = case g (Traversing f) of Traversing f' -> f'-}

--itraverseOf' :: (IFun f Int a b -> IFun f Int s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf' l iafb = case l (IFun iafb) of IFun isft -> isft 0


{-withTraversed_ :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_ l f = case l (A Traversed_ (\x -> x) (\_ b -> b)) of A Traversed_ x y -> f x y-}

{-cloneTraversed_ :: (s ~*. a) b t -> (s ~* a) b t-}
{-cloneTraversed_ l = withTraversed_ l (\x y p -> lens x y p)-}

{-withTraversed_' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_' -}

{-type (s ~*  a) b t = forall p. Traversed_ p => p a b -> p s t-}

type instance Rep (Traversing f) = f
instance Map f => Sieve (Traversing f) where sieve = runTraversing
instance Map f => Tabulated (Traversing f) where tabulateP = Traversing
