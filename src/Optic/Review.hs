module Optic.Review where
import Traversed

newtype Review a b = Review {runReview :: b}
instance Dimap Review where dimap _ g (Review b) = Review (g b)
instance MapR Review where rmap g (Review b) = Review (g b)
instance ComapL Review where colmap _ (Review b) = Review b
instance Traversed' Review where prism _ bt (Review b) = Review (bt b)
instance Pure (Review a) where pure = Review
instance Map (Review a ) where map f (Review b) = Review (f b)
instance MapIso (Review a ) where mapIso _ = map

_Review :: Dimap p => p (Review b b) (Review t t) -> p b t
_Review = dimap Review runReview

review :: (Review b b -> Review t t) -> b -> t
review = _Review
