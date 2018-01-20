module Utils.Lens (module Utils.Lens, module X) where
import Class.Lens as X

{-type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)-}
{-type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)-}

{-type (s ~*.  a) b t = A Lens a b a b -> A Lens a b s t-}
{-type  s ~**. a = A Lens a a a a -> A Lens a a s s-}

{-withLens :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withLens l f = case l (A Lens (\x -> x) (\_ b -> b)) of A Lens x y -> f x y-}

{-cloneLens :: (s ~*. a) b t -> (s ~* a) b t-}
{-cloneLens l = withLens l (\x y p -> lens x y p)-}

{-withLens' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withLens' -}

type (s ~*  a) b t = forall p. Lens p => p a b -> p s t

($:) :: Lens p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `rmap` first p

