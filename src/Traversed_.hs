module Traversed_ (module Traversed_, module X) where
import Traversed_.Class as X

{-type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)-}
{-type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)-}

{-type (s ~*.  a) b t = A Traversed_ a b a b -> A Traversed_ a b s t-}
{-type  s ~**. a = A Traversed_ a a a a -> A Traversed_ a a s s-}

{-withTraversed_ :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_ l f = case l (A Traversed_ (\x -> x) (\_ b -> b)) of A Traversed_ x y -> f x y-}

{-cloneTraversed_ :: (s ~*. a) b t -> (s ~* a) b t-}
{-cloneTraversed_ l = withTraversed_ l (\x y p -> lens x y p)-}

{-withTraversed_' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_' -}

type (s ~*  a) b t = forall p. Traversed_ p => p a b -> p s t

($:) :: Traversed_ p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `rmap` first p

