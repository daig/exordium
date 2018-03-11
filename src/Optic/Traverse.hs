module Optic.Traverse (module Optic.Traverse, module X) where
import Indexable.Class as X
import Map.Pro as X
import Closed as X

newtype Traversal f a b = Traversal {runTraversal :: a -> f b}

_Traversal :: (Traversal f a b -> Traversal f s t) -> (a -> f b) -> s -> f t
_Traversal l afb s = case l (Traversal afb) of Traversal sft -> sft s

{-instance Bind m => Compose (Traversal m) where Traversal f > Traversal g = Traversal (g <=< f)-}
{-instance Monad m => Category (Traversal m) where id = Traversal pure-}
instance Distribute f => Closed (Traversal f) where
  closed (Traversal afb) = Traversal (\xa -> distribute (\x -> afb (xa x)))
instance Map f => Promap (Traversal f) where promap f g (Traversal s) = Traversal (promap f (map g) s)
instance Map f => Map (Traversal f a) where map f (Traversal s) = Traversal (\a -> map f (s a))
{--- TODO: move to PromapIso class-}

instance Map f => Traversed_ (Traversal f) where traversal_ afbsft (Traversal afb) = Traversal (\s -> afbsft afb s)
instance Pure f => Traversed' (Traversal f) where
  prism pat constr (Traversal afb) = Traversal (\s -> case pat s of
    L t -> pure t
    R a -> constr `map` afb a)
instance Apply f => Traversed1 (Traversal f) where
  traversal1 afbsft (Traversal afb) = Traversal (\s -> afbsft afb s)
instance Pure f => Traversed0 (Traversal f) where
  traversal0 afbsft (Traversal afb) = Traversal (\s -> afbsft afb s)
instance Applicative f => Traversed (Traversal f) where
  traversal afbsft (Traversal afb) = Traversal (\s -> afbsft afb s)

instance Indexed i (Traversal f) where indexed s _ = s

instance Distribute f => Mapped (Traversal f) where
   mapped (Traversal f) = Traversal (collect f)


--itraverseOf :: Applicative f => (IndexingP (Traversal f) a b -> IndexingP (Traversal f) s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf l iafb = runTraversal (indexP (l (IndexingP (\i -> (i `plus` (1::Int),Traversal (iafb i))))) (0::Int))

{-collectOf :: (Traversal f a b -> Traversal f s t) -> (a -> f b) -> s -> f t-}
{-collectOf g f = case g (Traversal f) of Traversal f' -> f'-}
