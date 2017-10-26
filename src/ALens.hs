module ALens
  (type (*~.), type (*~~.)
  ,withLens, cloneLens
  ,module X) where
import Lens as X
import ALens.Shop as X (Shop)
import ALens.Shop

{-type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)-}
{-type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)-}

type (s *~.  a) b t = Shop a b a b -> Shop a b s t
type  s *~~. a = Shop a a a a -> Shop a a s s

withLens :: (s *~. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = case l (Shop (\x -> x) (\_ b -> b)) of Shop x y -> f x y

cloneLens :: (s *~. a) b t -> (s *~ a) b t
cloneLens l = withLens l (\x y p -> lens x y p)

{-withLens' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withLens' -}
