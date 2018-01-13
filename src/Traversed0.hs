module Traversed0
  (Traversed0(..)
  ,leftDefault, rightDefault
  ,module X) where
{-import Traversed as X-}
import Prism as X
import Lens as X
import Pure as X
import Traverse0 as X
import Baz
import I
import Sum

-- TODO: merge with Choice??
class (Prism p, Lens p) => Traversed0 p where
  traversal0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-wander0 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  traversed0 :: Traverse0 t => p a b -> p (t a) (t b)
  {-traversed0 = traversal0 traverse0-}
  lens0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  lens0 get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> either (\x -> x) (set s) bt) (first (right pab))

instance Traversed0 (->) where traversal0 l f s = case l (\a -> I (f a)) s of {I t -> t}

leftDefault :: Traversed0 p => p a b -> p (E a y) (E b y)
leftDefault = \p -> dimap swap swap (traversed0 p)
rightDefault :: Traversed0 p => p a b -> p (E x a) (E x b)
rightDefault = traversed0

{-type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t-}
{-type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s-}
