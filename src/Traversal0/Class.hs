module Traversal0.Class (module Traversal0.Class, module X) where
{-import Traversal.Class as X-}
import Prism.Class as X
import Traversed_.Class as X
import Pure.Class as X
import Traverse0.Class as X
import Traversal.Internal
import {-# source #-} I
import E
import Tuple
import Star.Type

-- TODO: merge with Choice??
class (Prism p, Traversed_ p) => Traversal0 p where
  traversal0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-traversal0 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  traversed0 :: Traverse0 t => p a b -> p (t a) (t b)
  traversed0 = traversal0 traverse0
  lens0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  lens0 get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> e'bifoldMap_ (\x -> x) (set s) bt) (first (right pab))

instance Traversal0 (->) where traversal0 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Pure f => Traversal0 (Star f) where traversal0 afbsft (Star afb) = Star (\s -> afbsft afb s)

{-type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t-}
{-type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s-}
