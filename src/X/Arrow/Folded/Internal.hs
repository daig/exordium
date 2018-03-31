module X.Arrow.Folded.Internal (module X.Arrow.Folded.Internal, module X) where
import X.Num.Add0 as X
import X.Functor.Empty as X
import X.Functor.Pure as X
import X.Functor.Fold as X
import X.Constraint.Trivial as X

newtype FreeFold c a = FreeFold {runFreeFold :: forall m. c m => (a -> m) -> m}
instance Add (FreeFold Add a) where
  FreeFold f `add` FreeFold g = FreeFold (\am -> add (f am) (g am))
instance Fold (FreeFold Add) where foldMap = foldMap1
instance Fold1 (FreeFold Add) where foldMap1 f = (`runFreeFold` f)
instance Map (FreeFold Add) where map f (FreeFold k) = FreeFold (\bm -> k (\a -> bm (f a)))
instance Remap (FreeFold Add) where remap _ = map
instance Pure (FreeFold Add) where pure a = FreeFold (\f -> f a)

instance Add0 (FreeFold Add0 a)
instance Add (FreeFold Add0 a) where
  FreeFold f `add` FreeFold g = FreeFold (\am -> add (f am) (g am))
instance Fold (FreeFold Add0) where foldMap f = (`runFreeFold` f)
instance Map (FreeFold Add0) where map f (FreeFold k) = FreeFold (\bm -> k (\a -> bm (f a)))
instance Remap (FreeFold Add0) where remap _ = map
instance Pure (FreeFold Add0) where pure a = FreeFold (\f -> f a)
instance Empty (FreeFold Add0) where empty = FreeFold (\_ -> zero)
instance Zero (FreeFold Add0 a) where zero = empty

instance Fold (FreeFold Zero) where foldMap f = (`runFreeFold` f)
instance Fold0 (FreeFold Zero) where foldMap0 f = (`runFreeFold` f)
instance Map (FreeFold Zero) where map f (FreeFold k) = FreeFold (\bm -> k (\a -> bm (f a)))
instance Remap (FreeFold Zero) where remap _ = map
instance Pure (FreeFold Zero) where pure a = FreeFold (\f -> f a)
instance Empty (FreeFold Zero) where empty = FreeFold (\_ -> zero)
instance Zero (FreeFold Zero a) where zero = empty

instance Fold (FreeFold Trivial) where foldMap = foldMap_
instance Fold0 (FreeFold Trivial) where foldMap0 = foldMap_
instance Fold1 (FreeFold Trivial) where foldMap1 = foldMap_
instance Fold_ (FreeFold Trivial) where foldMap_ f = (`runFreeFold` f)
instance Map (FreeFold Trivial) where map f (FreeFold k) = FreeFold (\bm -> k (\a -> bm (f a)))
instance Remap (FreeFold Trivial) where remap _ = map
instance Pure (FreeFold Trivial) where pure a = FreeFold (\f -> f a)
