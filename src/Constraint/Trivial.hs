{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Constraint.Trivial (Trivial,module X) where
import Kind.Constraint as X

-- | The trivial constraint, which is satisfied for every type.
class Trivial t
instance Trivial t
{-type family Trivial_ (t :: k) :: Constraint where Trivial_ t = ()-}
{-class Trivial_ t => Trivial t-}
{-instance Trivial_ t => Trivial t-}
