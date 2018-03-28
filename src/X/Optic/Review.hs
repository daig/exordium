module X.Optic.Review (module X.Optic.Review, module X) where
import X.Arrow.Precoerce as X

newtype Review a b = Review {runReview :: b}
instance Promap Review where promap _ g (Review b) = Review (g b)
instance Traversed' Review where prism _ bt (Review b) = Review (bt b)
instance Precoerce Review where precoerce (Review b) = Review b
instance Pure (Review a) where pure = Review
instance Map (Review a ) where map f (Review b) = Review (f b)


instance Fold (Review x) where foldMap = foldMap_
instance Fold0 (Review x) where foldMap0 = foldMap_
instance Fold1 (Review x) where foldMap1 = foldMap_
instance Fold_ (Review x) where foldMap_ = traverse__foldMap_
instance Comonad (Review x)
instance Duplicate (Review x) where duplicate = Review
instance Traverse (Review x) where traverse = traverse_
instance Traverse0 (Review x) where traverse0 = traverse_
instance Traverse1 (Review x) where traverse1 = traverse_
instance Traverse_ (Review x) where traverse_ afb (Review a) = Review `map` afb a

_Review :: Promap p => p (Review b b) (Review t t) -> p b t
_Review = promap Review runReview

review :: forall t b. (Review b b -> Review t t) -> b -> t
review = _Review


{-data Rev a b s t = Rev {runRev :: b -> t}-}
{-instance Promap (Rev a b) where promap _ g (Rev bt) = Rev (\b -> g (bt b))-}
{-instance Traversed' (Rev a b) where prism _ bt (Rev bb) = Rev (\b -> bt (bb b))-}
