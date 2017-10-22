module Applicative (Applicative, module X) where
import Pure as X
import Apply as X

class (Pure f, Apply f) => Applicative f
