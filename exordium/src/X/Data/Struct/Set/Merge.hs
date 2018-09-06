module X.Data.Struct.Set.Merge where

-- A version of Set with peculiar Semigroup and Monoid instances.
-- The result of xs <> ys will only be a valid set if the greatest
-- element of xs is strictly less than the least element of ys.
-- This is used to define cartesianProduct.
newtype MergeSet a = MergeSet { getMergeSet :: Set a }

#if (MIN_VERSION_base(4,9,0))
instance Semigroup (MergeSet a) where
  MergeSet xs <> MergeSet ys = MergeSet (merge xs ys)
#endif

instance Monoid (MergeSet a) where
  mempty = MergeSet empty

#if (MIN_VERSION_base(4,9,0))
  mappend = (<>)
#else
  mappend (MergeSet xs) (MergeSet ys) = MergeSet (merge xs ys)
#endif
 
