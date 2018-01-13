module AffTraversed
  (AffTraversed(..)
  ,leftDefault, rightDefault
  ,module X) where
{-import Traversed as X-}
import Prism as X
import Strong as X
import Pure as X
import AffTraverse as X
import Baz
import I
import Sum

-- TODO: merge with Choice??
class (Prism p, Strong p) => AffTraversed p where
  wander0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-wander0 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  traversed0 :: AffTraverse t => p a b -> p (t a) (t b)
  {-traversed0 = wander0 traverse0-}
  traversal0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  traversal0 get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> either (\x -> x) (set s) bt) (first (right pab))

instance AffTraversed (->) where wander0 l f s = case l (\a -> I (f a)) s of {I t -> t}

leftDefault :: AffTraversed p => p a b -> p (E a y) (E b y)
leftDefault = \p -> dimap swap swap (traversed0 p)
rightDefault :: AffTraversed p => p a b -> p (E x a) (E x b)
rightDefault = traversed0
