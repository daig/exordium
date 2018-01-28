module TraversalC (module TraversalC, module X) where
import Applicative.Class
import Dimap.Class as X
import Map.Class as X
import TraverseC as X
import K.Type
import I.Type
import Tuple
import Fun
import Star.Type

class Traversal c p | p -> c where
  {-lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t-}
  {-lens get set = \p -> dimap (\x -> (x,get x)) (\(s,b) -> set s b) (second p)-}
  {-lens get set = traversal_ (\afb s -> set s `map` afb (get s))-}
  traversal :: (forall f. c f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-traversal f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})-}
  traversed :: forall t a b. Traverse c t => p a b -> p (t a) (t b)
  traversed = traversal traverse
  {-second :: p a b -> p (x,a) (x,b)-}
  {-second = \p -> dimap swap swap (first p)-}
  {-second = traversed_-}
  {-first :: p a b -> p (a,y) (b,y)-}
  {-first = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {-first = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  {-first = \p -> dimap tuple'swap tuple'swap (second p)-}

{-traversal__dimap :: Lens p => (a -> x) -> (y -> b) -> p x y -> p a b-}
{-traversal__dimap f g = traversal_ (\xfy a -> map g (xfy (f a)))-}

instance Traversal Applicative (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}
{-instance Traversal Map (->) where-}
  {-lens = fun'lens-}
  {-first = fun'first-}
  {-second = fun'second-}

{-instance Map f => Lens (Star f) where traversal_ afbsft (Star afb) = Star (\s -> afbsft afb s)-}



{-instance Optic Lens where data A Lens a b s t = Lens (s -> a) (s -> b -> t)-}
{-instance Lens (A Lens a b) where-}
  {-first (Lens x y) = Lens (\(a,_) -> x a) (\(s,c) b -> (y s b,c))-}
{-instance Dimap (A Lens a b) where-}
  {-dimap f g (Lens x y) = Lens (\s -> x (f s)) (\s b -> g (y (f s) b))-}

{-data A Lens p a b t = Pretext {runPretext :: forall f. Map f => p a (f b) -> f t}-}
