module Constraint.Trivial (Trivial,module X) where
import Kind.Constraint as X

-- | The trivial constraint, which is satisfied for every type.
type family Trivial (t :: k) :: Constraint where Trivial t = ()
