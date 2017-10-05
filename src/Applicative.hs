module Applicative (Applicative, module X) where
import Pure as X
import Apply as X

type Applicative f = (Pure f, Apply f)
