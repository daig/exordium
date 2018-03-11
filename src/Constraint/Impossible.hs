module Constraint.Impossible (Impossible, module X) where
import Kind.Constraint as X

-- | The impossible constraint, which can never be satisfied.
type family Impossible (t :: k) :: Constraint where
