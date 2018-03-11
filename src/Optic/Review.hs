module Optic.Review where
import Traversed

newtype Review a b = Review {runReview :: b}
instance Promap Review where promap _ g (Review b) = Review (g b)
instance Traversed' Review where prism _ bt (Review b) = Review (bt b)
instance Pure (Review a) where pure = Review
instance Map (Review a ) where map f (Review b) = Review (f b)

_Review :: Promap p => p (Review b b) (Review t t) -> p b t
_Review = promap Review runReview

review :: forall t b. (Review b b -> Review t t) -> b -> t
review = _Review
