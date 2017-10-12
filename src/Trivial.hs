module Trivial (Trivial, module X) where
import Types as X (Constraint)

class Trivial a
instance Trivial a
