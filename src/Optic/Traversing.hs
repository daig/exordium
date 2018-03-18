module Optic.Traversing (module Optic.Traversing, module X) where
{-import Functor.Indexed-}
{-import Indexable.Class as X-}
import Arrow.Traversed as X
import Arrow.Mapped as X

newtype Traversing f a b = Traversing {runTraversing :: a -> f b}

_Traversing :: (Traversing f a b -> Traversing f s t) -> (a -> f b) -> s -> f t
_Traversing l afb s = case l (Traversing afb) of Traversing sft -> sft s

{-instance Bind m => Compose (Traversing m) where Traversing f > Traversing g = Traversing (g <=< f)-}
{-instance Monad m => Category (Traversing m) where id = Traversing pure-}
instance Distribute f => Closed (Traversing f) where
  closed (Traversing afb) = Traversing (\xa -> distribute (\x -> afb (xa x)))
instance Map f => Promap (Traversing f) where promap f g (Traversing s) = Traversing (promap f (map g) s)
instance Map f => Map (Traversing f a) where map f (Traversing s) = Traversing (\a -> map f (s a))
{--- TODO: move to PromapIso class-}

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

instance Distribute f => Mapped (Traversing f) where
   mapped (Traversing f) = Traversing (collect f)


--itraverseOf :: Applicative f => (IndexingP (Traversing f) a b -> IndexingP (Traversing f) s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf l iafb = runTraversing (indexP (l (IndexingP (\i -> (i `add` (1::Int),Traversing (iafb i))))) (0::Int))

{-collectOf :: (Traversing f a b -> Traversing f s t) -> (a -> f b) -> s -> f t-}
{-collectOf g f = case g (Traversing f) of Traversing f' -> f'-}
