{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
module Forall2 (module Forall2, module X) where
import Constraint.Type as X

type family Skolem2 (p :: k -> k' -> Constraint) :: k where
type family Skolem2' (p :: k -> k' -> Constraint) :: k' where
class p (Skolem2 p) (Skolem2' p) => Forall2 (p :: k -> k' -> Constraint)
instance p (Skolem2 p) (Skolem2' p) => Forall2 (p :: k -> k' -> Constraint)
class p (f a b) => Compose2 (p :: k'' -> Constraint) (f :: k -> k' -> k'') (a :: k) (b :: k')
instance p (f a b) => Compose2 p f a b
class Forall2 (Compose2 p f) => Forall2F (p :: k'' -> Constraint) (f :: k -> k' -> k'')
instance Forall2 (Compose2 p f) => Forall2F p f
